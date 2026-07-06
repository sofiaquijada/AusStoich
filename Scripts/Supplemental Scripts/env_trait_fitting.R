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

#this script is intended for assessing fit of LMM models
#notebook-type work: assessing singularity of NP and CP mods

#update: through this work realized colinearity issue in subsets of data
#end of script will delineate model selection


#--------------noodling around REML and ML
#will this fix singularity? check:
aus_data <- read_csv(file = "Inputs/aus_data2026.csv")
#try fixing skewness of SOP 
skewness(aus_data$SP_total_0_30) #5.194674
skewness(scale(aus_data$SP_total_0_30)) #same as above

skewness(log(aus_data$SP_total_0_30)) #1.442164
skewness(sqrt(aus_data$SP_total_0_30)) #3.226662
skewness((aus_data$SP_total_0_30)^(1/4)) #2.27701 
#so log gives smallest skew

#try to see if singularity issue is fixed: 

cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

aus_data$log_SP_total_0_30 <- log(aus_data$SP_total_0_30)

aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

cat_predictors <- c("AM", "EcM", "ErM", "NM", "woodiness", "putative_BNF", "reclass_life_history")
predictors <- c(cont_predictors, cat_predictors)
env <- as.data.frame(aus_data[predictors])
env <- env %>% mutate(across(all_of(cat_predictors), as.character))

#VIF
diag(solve(cor(env[cont_predictors])))

#highly colinear: AET-PPT, AET-temp_seasonality. Remove AET
env$AET <- NULL
cont_predictors <- cont_predictors[cont_predictors != "AET"]
diag(solve(cor(env[cont_predictors])))
corrplot(cor(aus_data[cont_predictors]))
#all VIFs below or marginally above 10 once AET removed
aus_data$AET <- NULL
diag(solve(cor(env[cont_predictors])))

traits <- c("ln_leaf_N", "ln_leaf_P", "leaf_C_per_dry_mass",
            "ln_NP_ratio", "ln_CN_ratio", "ln_CP_ratio")

response <- "ln_NP_ratio"


form1 <- as.formula(paste(response, "~",
                          paste(c(cat_predictors),
                                collapse = " + "),
                          "+ (1 | family/genus/species_binom)"))

model1 <- lmer(form1, data = aus_data, REML = FALSE)
isSingular(model1)
summary(model1) 

#singular with ln_NP_ratio
form2 <- as.formula(paste(response, "~",
                          paste(c("SN_total_0_30", "SOC_total_0_30",
                                  "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT",
                                  "precipitation_seasonality", "temp_seasonality", "log_SP_total_0_30",
                                  cat_predictors), collapse = " + ")))

#form2 <- as.formula(paste(response, "~",
#paste(c("SN_total_0_30", "SOC_total_0_30", "SP_total_0_30",
#  "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT",
# "precipitation_seasonality", "temp_seasonality",
# cat_predictors), collapse = " + ")))

model2 <- lm(form2, data = aus_data, )
summary(model2)

form3 <- as.formula(paste(response, "~",
                          paste(c(cont_predictors, cat_predictors), collapse = " + "),
                          "+ (1 | family/genus/species_binom)"))
model3 <- lme4::lmer(form3, data = aus_data, REML = FALSE)
summary(model3)
isSingular(model3)

#singularity not caused by SP skew. continue evaluating

#try fitting without family level, cat_tax for NP and CP

testmod <- lmer(ln_NP_ratio ~ AM+ EcM+ ErM+ NM+ woodiness+
                  putative_BNF+ reclass_life_history+
                  + (1 | family/genus/species_binom),
                data = aus_data, REML = FALSE)
#lme4 and lmer have dif class output, but same functions should in theory work

count(aus_data,is.na(ln_NP_ratio))
View(augment(ln_NP_ratio_cat_tax)) #this is input, fitted, and other diagnostics
augmented_NP <- augment(ln_NP_ratio_cat_tax)
summary(augmented_NP)

table(augmented_NP$reclass_life_history, augmented_NP$woodiness)
table(augmented_NP$family) #hm ok quite a few with only values of 1
count(as.data.frame(table(augmented_NP$family)), Freq == 1) #13 to be exact

table(augment(ln_NP_ratio_cat_env_tax)$family)

count(as.data.frame(table(augment(ln_NP_ratio_cat_env_tax)$family)),
      Freq == 1)#exactly the same

ln_NP_ratio_cat_tax_nofam <- lmer(ln_NP_ratio ~ AM+ EcM+ ErM+ NM+ woodiness+
                                    putative_BNF+ reclass_life_history+
                                    + (1 | genus/species_binom),
                                  data = aus_data, REML = FALSE)
summary(ln_NP_ratio_cat_tax_nofam) 
#this works, exactly same estimates except family term isn't included
#conclusion: NP probably doesn't have enough family-level variance among
#the categorical predictors to warrant deviance from global family intercept

#now look into CP:
summary(ln_CP_ratio_cat_tax)
#fixed effect model matrix rank def, ErM dropped
#family level variance is 0

augmented_CP <- augment(ln_CP_ratio_cat_tax)
table(augmented_CP$woodiness)


ln_CP_ratio_cat_tax_nofam <- lmer(ln_CP_ratio ~ AM+ EcM+ ErM+ NM+ woodiness+
                                    putative_BNF+ reclass_life_history+
                                    + (1 | genus/species_binom),
                                  data = aus_data, REML = FALSE)
summary(ln_CP_ratio_cat_tax_nofam)




#look into rank deficiency of CP term!!
#realized i need to look at colinearity of all predictors lol



#colinearity of predictors:
#1. env only        2. cat only       3. cat-env
#for each trait

#remove intercept term from model.matrix, then compute vif:
diag(solve(cor(model.matrix(ln_CP_ratio_cat_env_tax)[,-1])))

det(cor(model.matrix(ln_CP_ratio_cat_env_tax)[,-1])) 
corrplot(cor(model.matrix(ln_CP_ratio_cat_env_tax)[,-1]))
cor(model.matrix(ln_CP_ratio_cat_env_tax)[,-1])



#how many rows are both AM-NM and just AM and just NM? 
#probably correlation value for global dataset



#-------data exploration
#remember basic principles of model fitting, there should be reason to include a variable
#do plots of categorical variables by nutrient and nutrient ratio
#are there enough differences? what is the sample size by plot?

cat_predictors <- c(
  "AM", "EcM", "ErM", "NM",
  "woodiness",
  "putative_BNF",
  "reclass_life_history"
)

traits <- c(
  "leaf_C_per_dry_mass", 
  "ln_leaf_N",
  "ln_leaf_P",
  "ln_NP_ratio",
  "ln_CP_ratio",
  "ln_CN_ratio"
)

aus_data <- aus_data %>%
  mutate(across(all_of(cat_predictors), as.factor))

#long format
trait_long <- aus_data %>%
  select(all_of(cat_predictors), all_of(traits)) %>%
  pivot_longer(
    cols = all_of(traits),
    names_to = "trait",
    values_to = "value"
  )

myc_plot <- trait_long %>%
  pivot_longer(
    cols = c(AM, EcM, ErM, NM),
    names_to = "mycorrhizal_type",
    values_to = "presence"
  ) %>%
  filter(presence == 1) %>%
  ggplot(aes(x = mycorrhizal_type, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) +
  facet_wrap(~trait, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Mycorrhizal association",
    y = "Trait value"
  )

myc_plot

plot_trait_boxes_n <- function(data, cat_var, traits){
  
  long_dat <- data %>%
    select(all_of(cat_var), all_of(traits)) %>%
    pivot_longer(
      cols = all_of(traits),
      names_to = "trait",
      values_to = "value"
    )
  
  n_dat <- long_dat %>%
    group_by(.data[[cat_var]], trait) %>%
    summarise(n = sum(!is.na(value)), .groups = "drop")
  
  ggplot(long_dat,
         aes_string(x = cat_var, y = "value", fill = cat_var)) +
    
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    
    geom_jitter(
      width = 0.15,
      alpha = 0.25,
      size = 0.8
    ) +
    
    geom_text(
      data = n_dat,
      aes(
        x = .data[[cat_var]],
        y = Inf,
        label = paste0("n=", n)
      ),
      vjust = 1.2,
      inherit.aes = FALSE,
      size = 3
    ) +
    
    facet_wrap(~trait, scales = "free_y") +
    
    theme_bw() +
    
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

plot_trait_boxes_n(aus_data, "putative_BNF", traits)

#-----------------------colinearity assessment per trait

#global colinearity differs radically from subsets of data for each trait
#so really must do colinearity assessment on each subset

#exclude reclass life history..

#global---
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

aus_data$log_SP_total_0_30 <- log(aus_data$SP_total_0_30)

aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

#don't include life history
cat_predictors <- c("AM", "EcM", "ErM", "NM", "woodiness", "putative_BNF")
predictors <- c(cont_predictors, cat_predictors)
env <- as.data.frame(aus_data[predictors])

#VIF
diag(solve(cor(env[cont_predictors])))

#highly colinear: AET-PPT, AET-temp_seasonality. Remove AET
env$AET <- NULL
cont_predictors <- cont_predictors[cont_predictors != "AET"]
diag(solve(cor(env[cont_predictors])))
corrplot(cor(aus_data[cont_predictors]))
#all VIFs below or marginally above 10 once AET removed
#aus_data$AET <- NULL
diag(solve(cor(env[cont_predictors])))

#model.matrix for colinearity with categorical variables
env_complete <- env[complete.cases(env), ] 
env_complete[cat_predictors] <- lapply(env_complete[cat_predictors], as.factor)

env_cat <- model.matrix(~ ., data = env_complete) 
env_cat <- env_cat[,-1]
#compute variance inflation factors
diag(solve(cor(env_cat)))
det(cor(env_cat)) #the closer to 0 the worse it is
corrplot(cor(env_cat))


#NP ratio---

#reset cont predictors
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

NP_data <- subset(aus_data, !is.na(ln_NP_ratio))#does have logSP var
env_NP <- as.data.frame(NP_data[predictors])
env_NP[cat_predictors] <- lapply(env_NP[cat_predictors], as.factor)

#VIF
diag(solve(cor(envNP[cont_predictors])))
#again remove AET
env_NP$AET <- NULL
cont_predictors <- cont_predictors[cont_predictors != "AET"]
diag(solve(cor(env_NP[cont_predictors])))
corrplot(cor(env_NP[cont_predictors]))

env_NP <- env[complete.cases(env_NP), ] 
env_cat_NP <- model.matrix(~ ., data = env_NP) 
env_cat_NP <- env_cat[,-1]
#compute variance inflation factors
diag(solve(cor(env_cat_NP)))
det(cor(env_cat_NP)) #also close to 0
corrplot(cor(env_cat_NP))
rm(NP_data, env_NP, env_cat_NP)

#CP ratio---

#reset cont predictors
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

CP_data <- subset(aus_data, !is.na(ln_CP_ratio))#does have logSP var
env_CP <- as.data.frame(CP_data[predictors])
env_CP[cat_predictors] <- lapply(env_CP[cat_predictors], as.factor)

#VIF
diag(solve(cor(env_CP[cont_predictors])))
#here, SN, MAT, AET, PPT and temp seasonality are troublesome
corrplot(cor(env_CP[cont_predictors]))
cor(env_CP[cont_predictors])
#SN-SOC, SN-AP, SN-NPP, SN-MAT, 
#logSP-

#look at correlations
View(cor(env_CP[cont_predictors], use = "complete.obs") %>% 
  as.data.frame() %>% 
  rownames_to_column("var1") %>% 
  pivot_longer(-var1, names_to = "var2", values_to = "cor") %>% 
  filter(var1 < var2, abs(cor) > 0.5))

#how to filter?
#could also pick relevant variables and choose from those
library(usdm)
vifstep(env_CP[cont_predictors], th = 10)
#look inside

env_CP <- env[complete.cases(env), ] #idk why AET is removed?
env_cat_CP <- model.matrix(~ ., data = env_CP) 
#compute variance inflation factors
diag(solve(cor(env_cat_CP)))
det(cor(env_cat_CP)) #also close to 0
corrplot(cor(env_cat_CP))
#NM-AM correlated again...

#CN ratio ------
#reset cont predictors
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

CN_data <- subset(aus_data, !is.na(ln_CN_ratio))#does have logSP var
env_CN <- as.data.frame(CN_data[predictors])
env_CN[cat_predictors] <- lapply(env_CN[cat_predictors], as.factor)

#VIF
diag(solve(cor(env_CN[cont_predictors])))
corrplot(cor(env_CN[cont_predictors]))
cor(env_CN[cont_predictors])



#leaf N-----

#this should be case complete
#reset
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

N_data <- subset(aus_data, !is.na(ln_leaf_N))#does have logSP var
env_N <- as.data.frame(N_data[predictors])
env_N[cat_predictors] <- lapply(env_N[cat_predictors], as.factor)

#VIF
diag(solve(cor(env_N[cont_predictors]))) #should be the same as global
corrplot(cor(env_N[cont_predictors]))
cor(env_N[cont_predictors])



#leaf P------

#reset
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

P_data <- subset(aus_data, !is.na(ln_leaf_P))#does have logSP var
env_P <- as.data.frame(P_data[predictors])
env_P[cat_predictors] <- lapply(env_P[cat_predictors], as.factor)

#VIF
diag(solve(cor(env_P[cont_predictors]))) #should be the same as global
corrplot(cor(env_P[cont_predictors]))
cor(env_P[cont_predictors])



#leaf C----
#reset
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

C_data <- subset(aus_data, !is.na(leaf_C_per_dry_mass))#does have logSP var
env_C <- as.data.frame(C_data[predictors])
env_C[cat_predictors] <- lapply(env_C[cat_predictors], as.factor)

#VIF
diag(solve(cor(env_C[cont_predictors]))) #should be the same as global
corrplot(cor(env_C[cont_predictors]))
cor(env_C[cont_predictors])


#------------------------------Variable Selection-------------------------------

#exclude categorical variables
#this is draft of script that will be used in both lmms and MCMCglmm mods
#for each subset of trait

#copy script from borcard and legendre method variable selection:

library(adespatial)
library(usdm)
library(vegan)

# 1.Ordinary linear regression y ~ explanatory variables
#a) variable selection procedure: ordistep() from vegan (scope = forward)
#                                 forward.sel() from adespatial
#do both but eventualy only one
#uhh nvm these are for multivariate Y (response lol)
#b) colinearity. vif()

#want input of forward selection to be linearly indep so vifstep first

#1. subset predictor data on trait of interest
#2. vifstep (th = 10) to assess colinearity, remove colinear vars
#3. variable selection 


#variable selection is not necessarily to get rid of colinearity
#subsets of data can be very colinear, so address this
#then can do AIC or R2 based variable selection

#question: should i do variable selection before adjusting for colinearity? google this

#rabbit hole: use RF to do var selection with multicolinearity 
#or PCA to ID main drivers of clusters


#added log SP

#predictors
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

NP_data <- subset(aus_data, !is.na(ln_NP_ratio))#does have logSP var
env_NP <- as.data.frame(NP_data[cont_predictors])

#VIF
diag(solve(cor(env_NP[cont_predictors])))

#again remove AET
#env_NP$AET <- NULL
#cont_predictors <- cont_predictors[cont_predictors != "AET"]

corrplot(cor(env_NP[cont_predictors]))

#with logSP not SP
NP.vsurf <-VSURF(x = env_NP, y = NP_data$ln_NP_ratio)
NP.vsurf$varselect.thres
NP.vsurf$varselect.interp
NP.vsurf$varselect.pred
#CEC, AP, and temp_seasonality?

ggplot(data = NP_data) +
  geom_point(aes(x = CEC_total_0_30, y = ln_NP_ratio))

ggplot(data = NP_data) +
  geom_point(aes(x = AP_total_0_30, y = ln_NP_ratio))

ggplot(data = NP_data) +
  geom_point(aes(x = temp_seasonality, y = ln_NP_ratio))
#lowkey this makes little sense to me...


#try just PCA 
#ugh instead of RF ill probably just do these
biplot(prcomp(env_NP))
library(factoextra)

library(usdm)

fviz_pca_var(prcomp(env_NP), col.var = "black",
             repel = TRUE)

p <- corrplot(cor(env_NP[cont_predictors]))


vifstep(env_NP, th = 10, keep = )

#some metric of nitrogen and some metric of phosphorus
#doesnt matter which but AP makes more sense
#keep nitrogen and phos for sure if it comes down to it
#carbon... eh


edaphic_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30")
edaphic_NP <- as.data.frame(NP_data[edaphic_predictors])
fviz_pca_var(prcomp(edaphic_NP), col.var = "black",
             repel = TRUE)
#have variable selection script! to keep things neat and consistent between 
#lmms and MCMCglmm