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