library(lme4)
library(tidyverse)
library(corrplot)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(broom.mixed)
library(lmerTest) #to get p-values, Satterthwaite's method
library(purrr)


#Objective: we want to determine whether category is a significant predictor
#of leaf nutrient concentrations & their ratios

#set working directory as needed
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich")

#read in trait data
aus_data <- read_csv(file = "Inputs/aus_data2026.csv")

#scale continuous predictors for comparable estimates
cont_predictors <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")
aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

#set categorical predictors
cat_predictors <- c("AM", "EcM", "ErM", "NM", "woodiness", "putative_BNF", "reclass_life_history")
predictors <- c(cont_predictors, cat_predictors)

#-------------------------Variable selection------------------------------------
env <- as.data.frame(aus_data[predictors])
env <- env %>% mutate(across(all_of(cat_predictors), as.character))

#compute VIF for continuous predictors
diag(solve(cor(env[cont_predictors])))

#plot correlated variables
corrplot(cor(aus_data[cont_predictors]))

#highly colinear: AET-PPT, AET-temp_seasonality. Remove AET
env$AET <- NULL
cont_predictors <- cont_predictors[cont_predictors != "AET"]
diag(solve(cor(env[cont_predictors])))
corrplot(cor(aus_data[cont_predictors]))
#all VIFs below or marginally above 10 once AET removed
aus_data$AET <- NULL

#model matrix for colinearity between categorical and continuous variables
env_complete <- env[complete.cases(env), ] 
env_complete[cat_predictors] <- lapply(env_complete[cat_predictors], as.factor)

env_cat <- model.matrix(~ ., data = env_complete) 
env_cat <- env_cat[,-1]
#compute variance inflation factors
diag(solve(cor(env_cat)))
cov(env_cat)
corrplot(cor(env_cat)) #seems alright

#trait distributions to check normality assumption
ggplot(data = aus_data) +
  geom_histogram(mapping = aes(x = ln_leaf_N)) +
  theme_minimal()

#following loop will run analyses for all traits + outputs and diagnostics

#-----------------------------Analysis loop-------------------------------------
traits <- c("ln_leaf_N", "ln_leaf_P", "leaf_C_per_dry_mass",
            "ln_NP_ratio", "ln_CN_ratio", "ln_CP_ratio")

basepath <- "Results/Mixed & OLS models"

model_types <- c("cat_tax", "cat_env", "cat_env_tax")

for (response in traits) {
  
  for (model_type in model_types) {
    
    #create folder
    filepath <- file.path(basepath, response, model_type)
    dir.create(filepath, recursive = TRUE, showWarnings = FALSE)
    
    #build formula depending on model
    if (model_type == "cat_tax") {
      
      form <- as.formula(paste(response,
                               "~", paste(c(cat_predictors),
                                collapse = " + "),
                               "+ (1 | family/genus/species_binom)"))
      
      model <- lmer(form, data = aus_data, REML = TRUE)
      
    } else if (model_type == "cat_env") {
      
      form <- as.formula(
        paste(response, "~", paste(c(cont_predictors, cat_predictors), collapse = " + "))
      )
      
      model <- lm(form, data = aus_data)
      
    } else if (model_type == "cat_env_tax") {
      
      form <- as.formula(
        paste(
          response, "~",
          paste(c(cont_predictors, cat_predictors), collapse = " + "),
          "+ (1 | family/genus/species_binom)"
        )
      )
      
      model <- lmer(form, data = aus_data, REML = TRUE)
    }
    
    #save summary
    sink(file.path(filepath, paste0(response, "_", model_type, "_summary.txt")))
    print(summary(model))
    sink()
    
    #variance partitioning (only works for mixed models)
    if (inherits(model, "lmerMod")) {
      sink(file.path(filepath, paste0(response, "_", model_type, "_varpart.txt")))
      print(r.squaredGLMM(model))
      sink()
    }
    
    #save model object
    saveRDS(model,
            file = file.path(filepath,
                             paste0(response, "_", model_type, ".RDS")))
    
    #save broom outputs
    write.csv(glance(model),
              file = file.path(filepath,
                               paste0(response, "_", model_type, "_glance.csv")))
    
    write.csv(tidy(model, effects = "fixed"),
              file = file.path(filepath,
                               paste0(response, "_", model_type, "_tidy.csv")))
    
    write.csv(augment(model),
              file = file.path(filepath,
                               paste0(response, "_", model_type, "_augment.csv")))
  }
}

#compute diagnostics for 18 models
#-------------------------------Diagnostics-------------------------------------

#fill in depending on the RDS that is read in
#18 total model objects
mod <- ln_leaf_N_cat_env_tax 

#checks:
#homogeneity of residuals
plot(resid(mod) ~ fitted(mod))

#independence of residuals with each covariate
mf <- model.frame(mod)
rows_used <- as.numeric(rownames(mf))
res <- resid(mod)

cont_data <- aus_data[rows_used, cont_predictors]
cat_data  <- aus_data[rows_used, cat_predictors]

#looking for homogenous dispersion of residuals around 0
par(mfrow = c(3, 4), mar = c(4,4,2,1))
for (v in cont_predictors) {
  plot(cont_data[[v]], res, xlab = v)
  abline(h = 0, lty = 2)}

par(mfrow = c(2, 4), mar = c(4,4,2,1))
for (v in cat_predictors) {
  boxplot(res ~ cat_data[[v]], xlab = v)}

#normality of residuals
hist(resid(mod))

#amongst random levels, whether they are included or not
sp <- aus_data$species_binom[rows_used]
gen <- aus_data$genus[rows_used]
fam <- aus_data$family[rows_used]
par(mfrow = c(3, 1), mar = c(4,4,2,1))
boxplot(res ~ sp)
boxplot(res ~ gen)
boxplot(res ~ fam)

rm(mod)
#these results all summarized in env_trait_qmd

#----------------------------------Plots----------------------------------------

varpart_random <- function(mod) {
  #this function is meant to partition variance of nested random effects
  #modeled after Dynarski 2021 Ecology
  
  #get random-effect variances
  temp1 <- data.frame(VarCorr(mod))
  temp2 <- temp1 %>%
    rename(group = grp,
           variance = vcov) %>%
    mutate(group = fct_recode(group, 
                              "species_binom" = "species_binom:genus:family",
                              "genus" = "genus:family",
                              "family" = "family",
                              "residual" = "Residual"))
  #compute total variance
  tot_var <- temp2 %>% 
    summarize(sum_var = sum(variance))
  temp3 <- cbind(temp2, tot_var)
  
  #convert variances to percentages
  temp3 %>%
    mutate(pct_var = (variance/sum_var)*100) %>%
    select(group, pct_var)
  }

#---plots for cat_tax
#1. random effect varpart
N_part_CT <- varpart_random(ln_leaf_N_cat_tax)
P_part_CT <- varpart_random(ln_leaf_P_cat_tax)
C_part_CT <- varpart_random(leaf_C_per_dry_mass_cat_tax)
NP_part_CT <- varpart_random(ln_NP_ratio_cat_tax)
CN_part_CT <- varpart_random(ln_CP_ratio_cat_tax)
CP_part_CT <- varpart_random(ln_CN_ratio_cat_tax)

N_part_CT$foliar_trait  <- "pct_var_n"
P_part_CT$foliar_trait  <- "pct_var_p"
C_part_CT$foliar_trait  <- "pct_var_c"
NP_part_CT$foliar_trait <- "pct_var_np"
CN_part_CT$foliar_trait <- "pct_var_cn"
CP_part_CT$foliar_trait <- "pct_var_cp"

all_var_catax_random <- bind_rows(N_part_CT, P_part_CT, C_part_CT, 
                           NP_part_CT,CN_part_CT,CP_part_CT)

all_var_catax_random$group <- factor(all_var_catax_random$group,
  levels = c("residual","species_binom","genus","family"))

all_var_catax_random$foliar_trait <- factor(all_var_catax_random$foliar_trait,
  levels = c("pct_var_n","pct_var_p", "pct_var_c", "pct_var_np","pct_var_cp","pct_var_cn"))

ggplot(all_var_catax_random,
  aes(fill = group, y = pct_var, x = foliar_trait)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Foliar chemistry", y = "Percent of variance explained",
       title = "Variance Partition of Random Effect in trait ~ category + taxonomy mod") +
  scale_fill_brewer(name = "", breaks = rev(levels(all_var_catax_random$group)),
    labels = c("Family","Genus","Species","Residual"), palette = "Dark2") +
  scale_x_discrete(limits = rev(levels(all_var_catax_random$foliar_trait)),
                   labels = c("CN", "CP", "NP", "C", "P", "N")) + #set labels backwards to match the proper ordering
  coord_flip()


#taxonomic variance partition table (modeled after Dynarski)
var_part_table_catax <- all_var_catax_random %>%
  group_by(foliar_trait) %>%
  pivot_wider(names_from = group,
              values_from = pct_var) %>%
  mutate(taxonomic = family + genus + species_binom) %>%
  select(foliar_trait, taxonomic, residual) %>%
  mutate(across(where(is.numeric), round, 2))

#2. random + fixed varpart
#r.squaredGLMM() takes time to run, results for all mod saved in txt. file


scale_variance <- function(part_table, r2_file, trait_name){
  #varpart_random outputs variance from random effects only
  #r2_file is R2glmm filepath
  #part_table is output of varpart_random
  #this function takes output from varpart_random
  #and puts the random effect variance into R2m form i.e.
  #relative to fixed and random effect variance
  
  r2 <- read.table(r2_file, header = TRUE) #saved as txt bc faster
  
  R2m <- r2$R2m
  R2c <- r2$R2c
  
  random_total <- R2c - R2m
  residual_var <- 1 - R2c
  
  trait_scaled <- part_table %>%
    filter(group != "residual") %>%
    #scale proportion of each random effect
    #due to different denominators for R2c and varpart_random output
    mutate(prop = pct_var / sum(pct_var) * random_total) %>%
    select(group, prop)
  
  out <- bind_rows(
    trait_scaled,
    data.frame(group = "environment", prop = R2m),
    data.frame(group = "residual", prop = residual_var)
  )
  out$foliar_trait <- trait_name
  out
}

#store varpart_outputs
var_tables <- list(N  = N_part_CT, P  = P_part_CT, C  = C_part_CT, 
                   NP = NP_part_CT, CN = CN_part_CT, CP = CP_part_CT)

#R2glmm output locations
r2_files <- list(
  N  = "Results/Mixed_&_OLS models/ln_leaf_N/cat_tax/ln_leaf_N_cat_tax_varpart.txt",
  P  = "Results/Mixed_&_OLS models/ln_leaf_P/cat_tax/ln_leaf_P_cat_tax_varpart.txt",
  C  = "Results/Mixed_&_OLS models/leaf_C_per_dry_mass/cat_tax/leaf_C_per_dry_mass_cat_tax_varpart.txt",
  NP = "Results/Mixed_&_OLS models/ln_NP_ratio/cat_tax/ln_NP_ratio_cat_tax_varpart.txt",
  CN = "Results/Mixed_&_OLS models/ln_CN_ratio/cat_tax/ln_CN_ratio_cat_tax_varpart.txt",
  CP = "Results/Mixed_&_OLS models/ln_CP_ratio/cat_tax/ln_CP_ratio_cat_tax_varpart.txt")

#apply scale_variance to all traits
all_var_catax_total <- map_dfr(names(var_tables),
  function(trait){scale_variance(var_tables[[trait]], r2_files[[trait]], trait)
  })

all_var_catax_total$group <- factor(all_var_catax_total$group,
                                    levels = c("residual","environment","species_binom","genus","family"))
all_var_catax_total$foliar_trait <- factor(all_var_catax_total$foliar_trait,levels = c("N","P","C","NP","CN","CP"))

ggplot(all_var_catax_total, aes(x = foliar_trait, y = prop, fill = group)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Foliar chemistry",y = "Proportion of total variance", 
       title = "Variance Partition for trait ~ category + taxonomy mod") +
  scale_fill_brewer(
    palette = "Dark2",
    labels = c("Residual","Environment","Species","Genus","Family")
  )

#try and get consistent colors
var_colors <- c(residual = "#1B9E77",environment = "#D95F02",
  species_binom = "#7570B3", genus = "#E7298A",family = "#66A61E")

ggplot(all_var_catax_random,
       aes(fill = group, y = pct_var, x = foliar_trait)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Foliar chemistry", y = "Percent of variance explained",
       title = "Variance Partition of Random Effect in trait ~ category + taxonomy mod") +
  scale_fill_manual(
    values = var_colors[c("family","genus","species_binom","residual")],
    breaks = c("family","genus","species_binom","residual"),
    labels = c("Family","Genus","Species","Residual")
  ) +
  scale_x_discrete(
    limits = rev(levels(all_var_catax_random$foliar_trait)),
    labels = c("CN","CP","NP","C","P","N")
  ) +
  coord_flip()

ggplot(all_var_catax_total,
       aes(x = foliar_trait, y = prop, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Foliar chemistry",
       y = "Proportion of total variance",
       title = "Variance Partition for trait ~ category + taxonomy mod") +
  scale_fill_manual(
    values = var_colors,
    breaks = c("residual","environment","species_binom","genus","family"),
    labels = c("Residual","Environment","Species","Genus","Family")
  )

#--plots for cat_env_tax

#1. random effect varpart
N_part_ECT <- varpart_random(ln_leaf_N_cat_env_tax)
P_part_ECT <- varpart_random(ln_leaf_P_cat_env_tax)
C_part_ECT <- varpart_random(leaf_C_per_dry_mass_cat_env_tax)
NP_part_ECT <- varpart_random(ln_NP_ratio_cat_env_tax)
CN_part_ECT <- varpart_random(ln_CP_ratio_cat_env_tax)
CP_part_ECT <- varpart_random(ln_CN_ratio_cat_env_tax)

N_part_ECT$foliar_trait  <- "pct_var_n"
P_part_ECT$foliar_trait  <- "pct_var_p"
C_part_ECT$foliar_trait  <- "pct_var_c"
NP_part_ECT$foliar_trait <- "pct_var_np"
CN_part_ECT$foliar_trait <- "pct_var_cn"
CP_part_ECT$foliar_trait <- "pct_var_cp"

all_var_envcatax_random <- bind_rows(N_part_ECT, P_part_ECT, C_part_ECT, 
                                  NP_part_ECT,CN_part_ECT,CP_part_ECT)

all_var_envcatax_random$group <- factor(all_var_envcatax_random$group,
                                     levels = c("residual","species_binom","genus","family"))

all_var_envcatax_random$foliar_trait <- factor(all_var_envcatax_random$foliar_trait,
                                            levels = c("pct_var_n","pct_var_p", "pct_var_c", "pct_var_np","pct_var_cp","pct_var_cn"))

#2. random + fixed varpart
#store varpart_outputs
var_tables2 <- list(N  = N_part_ECT, P  = P_part_ECT, C  = C_part_ECT, 
                   NP = NP_part_ECT, CN = CN_part_ECT, CP = CP_part_ECT)

#R2glmm output locations
r2_files2 <- list(
  N  = "Results/Mixed_&_OLS models/ln_leaf_N/cat_env_tax/ln_leaf_N_cat_env_tax_varpart.txt",
  P  = "Results/Mixed_&_OLS models/ln_leaf_P/cat_env_tax/ln_leaf_P_cat_env_tax_varpart.txt",
  C  = "Results/Mixed_&_OLS models/leaf_C_per_dry_mass/cat_env_tax/leaf_C_per_dry_mass_cat_env_tax_varpart.txt",
  NP = "Results/Mixed_&_OLS models/ln_NP_ratio/cat_env_tax/ln_NP_ratio_cat_env_tax_varpart.txt",
  CN = "Results/Mixed_&_OLS models/ln_CN_ratio/cat_env_tax/ln_CN_ratio_cat_env_tax_varpart.txt",
  CP = "Results/Mixed_&_OLS models/ln_CP_ratio/cat_env_tax/ln_CP_ratio_cat_env_tax_varpart.txt")

#apply scale_variance to all traits
all_var_envcatax_total <- map_dfr(names(var_tables2),
                               function(trait){scale_variance(var_tables2[[trait]], r2_files2[[trait]], trait)
                               })

all_var_envcatax_total$group <- factor(all_var_envcatax_total$group,
                                    levels = c("residual","environment","species_binom","genus","family"))
all_var_envcatax_total$foliar_trait <- factor(all_var_envcatax_total$foliar_trait,levels = c("N","P","C","NP","CN","CP"))

#try and get consistent colors
var_colors <- c(residual = "#1B9E77",environment = "#D95F02",
                species_binom = "#7570B3", genus = "#E7298A",family = "#66A61E")

ggplot(all_var_envcatax_random,
       aes(fill = group, y = pct_var, x = foliar_trait)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Foliar chemistry", y = "Percent of variance explained",
       title = "Variance Partition of Random Effect in trait ~ category + environment + taxonomy mod") +
  scale_fill_manual(
    values = var_colors[c("family","genus","species_binom","residual")],
    breaks = c("family","genus","species_binom","residual"),
    labels = c("Family","Genus","Species","Residual")
  ) +
  scale_x_discrete(
    limits = rev(levels(all_var_catax_random$foliar_trait)),
    labels = c("CN","CP","NP","C","P","N")
  ) +
  coord_flip()

ggplot(all_var_envcatax_total,
       aes(x = foliar_trait, y = prop, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Foliar chemistry",
       y = "Proportion of total variance",
       title = "Variance Partition of Random Effect in trait ~ category + environment + taxonomy mod") +
  scale_fill_manual(
    values = var_colors,
    breaks = c("residual","environment","species_binom","genus","family"),
    labels = c("Residual","Environment","Species","Genus","Family")
  )

#---plot model estimates
#plot estimates based on significance 
#3 models per plot, so total 6 plots

plot_coef_models <- function(cat_tax_file, cat_env_file, cat_tax_env_file, trait_name){
  #this function takes as input filepaths for tidy model outputs
  #to plot 3 graphs for each model type in one plot
  
  cat_tax <- read_csv(cat_tax_file) %>% mutate(model = "Categories + Taxonomy")
  
  cat_env <- read_csv(cat_env_file) %>% mutate(model = "Cateogries + Environment")
  
  cat_tax_env <- read_csv(cat_tax_env_file) %>% mutate(model = "Categories + Environment + Taxonomy")
  
  all_mods <- bind_rows(cat_tax, cat_env, cat_tax_env) %>%
    filter(term != "(Intercept)") %>%
    #assuming 1.96 is ok to use for this
    mutate(lower = estimate - 1.96 * std.error, upper = estimate + 1.96 * std.error,
           sig = p.value < 0.05)
  
  # predictor labels
  term_labels <- c(
    woodiness = "Woodiness", reclass_life_historyshort = "Life History (short)",
    putative_BNF = "Nitrogen Fixation",
    NM = "NM", ErM = "ErM", EcM = "EcM", AM = "AM",
    temp_seasonality = "Temperature Seasonality",
    SP_total_0_30 = "Total Soil Phosphorus", SOC_total_0_30 = "Total Soil Carbon",
    SN_total_0_30 = "Total Soil Nitrogen",
    precipitation_seasonality = "Precipitation Seasonality",
    PPT = "Precipitation",
    NPP = "Net Primary Productivity",
    MAT = "Mean Annual Temperature",
    CEC_total_0_30 = "Cation Exchange Capacity",
    AP_total_0_30 = "Available Phosphorus")
  
  #desired ordering
  predictor_order <- c("woodiness","reclass_life_historyshort","putative_BNF",
    "NM","ErM","EcM","AM",
    "temp_seasonality","SP_total_0_30","SOC_total_0_30","SN_total_0_30",
    "precipitation_seasonality","PPT","NPP","MAT","CEC_total_0_30","AP_total_0_30")
  
  all_mods$term <- factor(all_mods$term, levels = predictor_order)
  
  pd <- position_dodge(width = 0.6) #to avoid overlap
  
  ggplot(all_mods, aes(x = estimate, y = term, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lower, xmax = upper, linewidth = sig),
      position = pd, height = 0) +
    geom_point(position = pd, size = 3) +
    scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.4),
      guide = "none") +
    scale_y_discrete(labels = term_labels) +
    labs(x = "Estimate", y = "", color = "Model", title = trait_name) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 12),
      legend.text = element_text(size = 13), legend.title = element_text(size = 14),
      plot.title = element_text(size = 15))
  }

plot_coef_models(
  "Results/Mixed_&_OLS models/ln_leaf_N/cat_tax/ln_leaf_N_cat_tax_tidy.csv",
  "Results/Mixed_&_OLS models/ln_leaf_N/cat_env/ln_leaf_N_cat_env_tidy.csv",
  "Results/Mixed_&_OLS models/ln_leaf_N/cat_env_tax/ln_leaf_N_cat_env_tax_tidy.csv",
  "Leaf N")

plot_coef_models(
    "Results/Mixed_&_OLS models/ln_leaf_P/cat_tax/ln_leaf_P_cat_tax_tidy.csv",
    "Results/Mixed_&_OLS models/ln_leaf_P/cat_env/ln_leaf_P_cat_env_tidy.csv",
    "Results/Mixed_&_OLS models/ln_leaf_P/cat_env_tax/ln_leaf_P_cat_env_tax_tidy.csv",
    "Leaf P")

plot_coef_models(
    "Results/Mixed_&_OLS models/leaf_C_per_dry_mass/cat_tax/leaf_C_per_dry_mass_cat_tax_tidy.csv",
    "Results/Mixed_&_OLS models/leaf_C_per_dry_mass/cat_env/leaf_C_per_dry_mass_cat_env_tidy.csv",
    "Results/Mixed_&_OLS models/leaf_C_per_dry_mass/cat_env_tax/leaf_C_per_dry_mass_cat_env_tax_tidy.csv",
    "Leaf C")

plot_coef_models(
  "Results/Mixed_&_OLS models/ln_NP_ratio/cat_tax/ln_NP_ratio_cat_tax_tidy.csv",
  "Results/Mixed_&_OLS models/ln_NP_ratio/cat_env/ln_NP_ratio_cat_env_tidy.csv",
  "Results/Mixed_&_OLS models/ln_NP_ratio/cat_env_tax/ln_NP_ratio_cat_env_tax_tidy.csv",
  "N:P Ratio")

plot_coef_models(
  "Results/Mixed_&_OLS models/ln_CN_ratio/cat_tax/ln_CN_ratio_cat_tax_tidy.csv",
  "Results/Mixed_&_OLS models/ln_CN_ratio/cat_env/ln_CN_ratio_cat_env_tidy.csv",
  "Results/Mixed_&_OLS models/ln_CN_ratio/cat_env_tax/ln_CN_ratio_cat_env_tax_tidy.csv",
  "C:N Ratio")

plot_coef_models(
  "Results/Mixed_&_OLS models/ln_CP_ratio/cat_tax/ln_CP_ratio_cat_tax_tidy.csv",
  "Results/Mixed_&_OLS models/ln_CP_ratio/cat_env/ln_CP_ratio_cat_env_tidy.csv",
  "Results/Mixed_&_OLS models/ln_CP_ratio/cat_env_tax/ln_CP_ratio_cat_env_tax_tidy.csv",
  "C:P Ratio")

#add AIC to the corner of each manually


#try:
remotes::install_github("mastoffel/partR2") 
library(partR2)
summary(partR2(ln_leaf_N_cat_tax))
#this is just fixed but probably doesn't work since fixed is so small
#could be worthwhile for CP

#-------------scrap-
#for mixed models, check if random effects are necessary
#create linear model without random effect, calculate its residuals
#plot residuals against levels of random factors
cat_env_lm_resid <- rstandard(cat_env_lm)
#depends on how many obs were used

mf <- model.frame(cat_env_lm)
rows_used <- as.numeric(rownames(mf))

res <- residuals(cat_env_lm)
sp <- aus_data$species_binom[rows_used]
boxplot(res ~ sp)
#we know random effects beneficial, so overkill to do this for all