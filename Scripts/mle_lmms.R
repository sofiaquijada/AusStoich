library(lme4)
library(tidyverse)
library(corrplot)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(broom.mixed)
library(lmerTest) #to get p-values, Satterthwaite's method
#note that lmerTest overrides lme4 lmer() function
library(purrr)
library(moments)

#this script will host lmm analyses with taxonomical random effect

#can have env tax and tax only !! do var part within code itself
#will the singularity issues persist? tbd
#will they run quicker bc now not everything is super colinear? also tbd

#set working directory as needed
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich")

#read in trait data
aus_data <- read_csv(file = "Inputs/aus_data2026.csv")

#scale continuous predictors for comparable estimates
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

aus_data$log_SP_total_0_30 <- log(aus_data$SP_total_0_30)

#scale after logging SP
aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

#variable selection done in qmd file, outputs read in here
selected_variables <- readRDS("Results/Summer 2026 Analyses/selected_variables.rds")

#define responses
traits <- c("ln_leaf_N", "ln_leaf_P", "leaf_C_per_dry_mass",
            "ln_NP_ratio", "ln_CN_ratio", "ln_CP_ratio")

#trait histograms to check normality assumption
for (response in traits) {
  p <- ggplot(data = aus_data) +
    geom_histogram(mapping = aes(x = .data[[response]])) +
    theme_minimal()
  plot(p)
  rm(p)
  }

#--------------------------------Analysis loop----------------------------------

basepath <- "Results/Summer 2026 Analyses/LMMs"
 
#model_types <- c("env_tax", "tax")
model_types <- c("tax")

for (response in traits) {
  
  #read in selected (linearly indep) variables
  current_cont_predictors <- selected_variables[[response]]
  
  for (model_type in model_types) {
    
    #create folder
    filepath <- file.path(basepath, response, model_type)
    dir.create(filepath, recursive = TRUE, showWarnings = FALSE)
  
  #build formula depending on model, use MLE estimation
  if (model_type == "env_tax") {
    
    form <- as.formula(paste(response,
                             "~", paste(c(current_cont_predictors),
                                        collapse = " + "),
                             "+ (1 | family/genus/species_binom)"))
    
    model <- lmer(form, data = aus_data, REML = FALSE)
    
  } else if (model_type == "tax") {
    
    form <- as.formula(
      paste(response, "~", ("1 | family/genus/species_binom"))
    )
    
    model <- lmer(form, data = aus_data, REML = FALSE)
    
  }
  
  #save summary
  sink(file.path(filepath, paste0(response, "_", model_type, "_summary.txt")))
  print(summary(model))
  sink()
  
  #save variance partitioning
  sink(file.path(filepath, paste0(response, "_", model_type, "_varpart.txt")))
  print(r.squaredGLMM(model))
  sink()
  
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

#-------------------------------Diagnostics-------------------------------------

#ran loop on GIC teal, follow up to diagnostics

#fill in depending on the RDS that is read in
#12 total model objects
mod <- ln_leaf_N_tax 

#checks:
#homogeneity of residuals
plot(resid(mod) ~ fitted(mod))

#independence of residuals with each covariate
mf <- model.frame(mod)
rows_used <- as.numeric(rownames(mf))
res <- resid(mod)

cont_data <- aus_data[rows_used, cont_predictors]

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

rm(mod, mf, rows_used, res, sp, gen, fam)
#these results all summarized in mle_lmms_diagnostic.qmd

#-------------------------------Results-----------------------------------------
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

#----------- plots for tax only

#1. random effect varpart
N_part <- varpart_random(ln_leaf_N_tax)
P_part <- varpart_random(ln_leaf_P_tax)
C_part <- varpart_random(leaf_C_per_dry_mass_tax)
NP_part <- varpart_random(ln_NP_ratio_tax)
CN_part <- varpart_random(ln_CP_ratio_tax)
CP_part <- varpart_random(ln_CN_ratio_tax)

N_part$foliar_trait  <- "pct_var_n"
P_part$foliar_trait  <- "pct_var_p"
C_part$foliar_trait  <- "pct_var_c"
NP_part$foliar_trait <- "pct_var_np"
CN_part$foliar_trait <- "pct_var_cn"
CP_part$foliar_trait <- "pct_var_cp"

random_parts_tax <- bind_rows(N_part, P_part, C_part, 
                              NP_part,CN_part,CP_part)

random_parts_tax$group <- factor(random_parts_tax$group,
                                 levels = c("residual","species_binom","genus","family"))

random_parts_tax$foliar_trait <- factor(random_parts_tax$foliar_trait,
                                        levels = c("pct_var_n","pct_var_p", "pct_var_c", "pct_var_np","pct_var_cp","pct_var_cn"))

ggplot(random_parts_tax,
       aes(fill = group, y = pct_var, x = foliar_trait)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Foliar Chemistry", y = "Percent of variance explained",
       title = "Variance Partition of Random Effect in trait ~ taxonomy mod") +
  scale_fill_manual(
    values = var_colors[c("family","genus","species_binom","residual")],
    breaks = c("family","genus","species_binom","residual"),
    labels = c("Family","Genus","Species","Residual")
  ) +
  scale_x_discrete(
    limits = rev(levels(random_parts_tax$foliar_trait)),
    labels = c("CN","CP","NP","C","P","N")
  ) +
  coord_flip() + 
  theme(
    axis.title = element_text(size = 0), #don't want axis titles for now
    axis.text.y = element_text(size = 14),  
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 20)
  )

rm(N_part, P_part, C_part, NP_part, CN_part, CP_part)

#----------- plots for env_tax

#1. random effect varpart
N_part_ET <- varpart_random(ln_leaf_N_env_tax)
P_part_ET <- varpart_random(ln_leaf_P_env_tax)
C_part_ET <- varpart_random(leaf_C_per_dry_mass_env_tax)
NP_part_ET <- varpart_random(ln_NP_ratio_env_tax)
CN_part_ET <- varpart_random(ln_CP_ratio_env_tax)
CP_part_ET <- varpart_random(ln_CN_ratio_env_tax)

N_part_ET$foliar_trait  <- "pct_var_n"
P_part_ET$foliar_trait  <- "pct_var_p"
C_part_ET$foliar_trait  <- "pct_var_c"
NP_part_ET$foliar_trait <- "pct_var_np"
CN_part_ET$foliar_trait <- "pct_var_cn"
CP_part_ET$foliar_trait <- "pct_var_cp"

random_parts_envtax <- bind_rows(N_part_ET, P_part_ET, C_part_ET, 
                                NP_part_ET,CN_part_ET,CP_part_ET)

random_parts_envtax$group <- factor(random_parts_envtax$group,
                                        levels = c("residual","species_binom","genus","family"))

random_parts_envtax$foliar_trait <- factor(random_parts_envtax$foliar_trait,
                                        levels = c("pct_var_n","pct_var_p", "pct_var_c", "pct_var_np","pct_var_cp","pct_var_cn"))

#2. random + fixed varpart
#store varpart_outputs
var_tables_ET <- list(N  = N_part_ET, P  = P_part_ET, C  = C_part_ET, 
                    NP = NP_part_ET, CN = CN_part_ET, CP = CP_part_ET)

#R2glmm output locations
r2_files_ET <- list(
  N  = "Results/Summer 2026 Analyses/LMMs/ln_leaf_N/env_tax/ln_leaf_N_env_tax_varpart.txt",
  P  = "Results/Summer 2026 Analyses/LMMs/ln_leaf_P/env_tax/ln_leaf_P_env_tax_varpart.txt",
  C  = "Results/Summer 2026 Analyses/LMMs/leaf_C_per_dry_mass/env_tax/leaf_C_per_dry_mass_env_tax_varpart.txt",
  NP = "Results/Summer 2026 Analyses/LMMs/ln_NP_ratio/env_tax/ln_NP_ratio_env_tax_varpart.txt",
  CN = "Results/Summer 2026 Analyses/LMMs/ln_CN_ratio/env_tax/ln_CN_ratio_env_tax_varpart.txt",
  CP = "Results/Summer 2026 Analyses/LMMs/ln_CP_ratio/env_tax/ln_CP_ratio_env_tax_varpart.txt")

#apply scale_variance to all traits
total_parts_envtax <- map_dfr(names(var_tables_ET),
                                  function(trait){scale_variance(var_tables_ET[[trait]], r2_files_ET[[trait]], trait)
                                  })

total_parts_envtax$group <- factor(total_parts_envtax$group,
                                       levels = c("residual","environment","species_binom","genus","family"))
total_parts_envtax$foliar_trait <- factor(total_parts_envtax$foliar_trait,levels = c("N","P","C","NP","CN","CP"))

#try and get consistent colors
var_colors <- c(residual = "#1B9E77",environment = "#D95F02",
                species_binom = "#7570B3", genus = "#E7298A",family = "#66A61E")

ggplot(random_parts_envtax,
       aes(fill = group, y = pct_var, x = foliar_trait)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Foliar chemistry", y = "Percent of variance explained",
       title = "Variance Partition of Random Effect in trait ~ environment + taxonomy mod") +
  scale_fill_manual(
    values = var_colors[c("family","genus","species_binom","residual")],
    breaks = c("family","genus","species_binom","residual"),
    labels = c("Family","Genus","Species","Residual")
  ) +
  scale_x_discrete(
    limits = rev(levels(random_parts_envtax$foliar_trait)),
    labels = c("CN","CP","NP","C","P","N")
  ) +
  coord_flip() +
  theme(
    axis.title = element_text(size = 0),
    axis.text.y = element_text(size = 16),   
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 16)
  )

ggplot(total_parts_envtax,
       aes(x = foliar_trait, y = prop, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Foliar chemistry",
       y = "Proportion of total variance",
       title = "Variance Partition in trait ~ environment + taxonomy mod") +
  scale_fill_manual(
    values = var_colors,
    breaks = c("residual","environment","species_binom","genus","family"),
    labels = c("Residual","Environment","Species","Genus","Family")
  ) +
  scale_x_discrete(
    limits = rev(levels(total_parts_envtax$foliar_trait)),
    labels = c("CN","CP","NP","C","P","N")) +
  theme(
    axis.title = element_text(size = 0),
    axis.text.y = element_text(size = 16),   
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 16)
  )

rm(N_part_ET, P_part_ET, C_part_ET, NP_part_ET, CN_part_ET, CP_part_ET)

#----------- plot model estimates

#plot estimates based on significance
#only for env tax model

plot_coefs <- function(model, env_tax_file, trait_name){
  #this function takes as input filepaths for tidy model output
  #to plot estimates
  
  #read in results and remove intercept
  env_tax <- read_csv(env_tax_file) %>%
    mutate(model = "Environment + Taxonomy") %>% filter(term != "(Intercept)")
  
  #confidence interval
  CI <- confint.merMod(model, method = "Wald") %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    rename(
      lower = "2.5 %",
      upper = "97.5 %")
  
  env_tax <- env_tax %>%
    left_join(CI, by = "term")
  
  #predictor labels
  term_labels <- c( AET = "Actual Evapotranspiration",
    temp_seasonality = "Temperature Seasonality",
    precipitation_seasonality = "Precipitation Seasonality",
    PPT = "Precipitation",
    NPP = "Net Primary Productivity",
    MAT = "Mean Annual Temperature",
    CEC_total_0_30 = "Cation Exchange Capacity",
    AP_total_0_30 = "Available Phosphorus",
    log_SP_total_0_30 = "Total Soil Phosphorus (log)", SOC_total_0_30 = "Total Soil Carbon",
    SN_total_0_30 = "Total Soil Nitrogen")
  
  #desired ordering
  predictor_order <- c("temp_seasonality", "precipitation_seasonality","PPT","NPP","MAT",
                       "CEC_total_0_30","AP_total_0_30",
                       "log_SP_total_0_30","SOC_total_0_30","SN_total_0_30" )

  env_tax$term <- factor(env_tax$term, levels = predictor_order)
  
  pd <- position_dodge(width = 0.6) #to avoid overlap
  
  ggplot(env_tax, aes(x = estimate, y = term)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), 
                   position = pd,height = 0) +
    geom_point(position = pd, size = 3) +
    scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.4), guide = "none") +
    scale_y_discrete(labels = term_labels) +
    labs(x = "Estimate",y = "", title = trait_name) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 15)
    )
}

plot_coefs(ln_leaf_N_env_tax, "Results/Summer 2026 Analyses/LMMs/ln_leaf_N/env_tax/ln_leaf_N_env_tax_tidy.csv",
          "Leaf N")
plot_coefs(ln_leaf_P_env_tax, "Results/Summer 2026 Analyses/LMMs/ln_leaf_P/env_tax/ln_leaf_P_env_tax_tidy.csv",
           "Leaf P")
plot_coefs(leaf_C_per_dry_mass_env_tax, "Results/Summer 2026 Analyses/LMMs/leaf_C_per_dry_mass/env_tax/leaf_C_per_dry_mass_env_tax_tidy.csv",
           "Leaf C")
plot_coefs(ln_NP_ratio_env_tax, "Results/Summer 2026 Analyses/LMMs/ln_NP_ratio/env_tax/ln_NP_ratio_env_tax_tidy.csv",
           "N:P ratio")
plot_coefs(ln_CN_ratio_env_tax, "Results/Summer 2026 Analyses/LMMs/ln_CN_ratio/env_tax/ln_CN_ratio_env_tax_tidy.csv",
           "C:N ratio")
plot_coefs(ln_CP_ratio_env_tax, "Results/Summer 2026 Analyses/LMMs/ln_CP_ratio/env_tax/ln_CP_ratio_env_tax_tidy.csv",
           "C:P ratio")

#CN most substantially explained by env
library(partR2)
summary(partR2(ln_CN_ratio_env_tax))