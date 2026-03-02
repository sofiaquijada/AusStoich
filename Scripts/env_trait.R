library(lme4)
library(tidyverse)
library(dplyr)
library(ggplot2)


#this script is intended for business as usual LM that includes all fixed factors
#get R2 for each predictor in this basic LM
#then basic lmm with species as random effect, then nested taxonomy as RF, again get R2 
#then compare all three with AIC or some other method

cont_predictors <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

cat_predictors <- c("myc_type", "woodiness", "putative_BNF", "reclass_life_history")

predictors <- c(cont_predictors, cat_predictors)

aus_data[cont_predictors] <- scale(aus_data[cont_predictors])


# Example: trait is your response, cont_predictors and cat_predictors are lists of column names
formula_fixed <- as.formula(
  paste("ln_NP_ratio ~", paste(c(cont_predictors, cat_predictors), collapse = " + "))
)

# 1. OLS (fixed effects only)
ols_mod <- lm(formula_fixed, data = aus_data)
summary(ols_mod)$r.squared   # R² for OLS
AIC(ols_mod)

# 2. Mixed model (taxonomy random effects, nested structure)
mixed_mod <- lmer(
  update(formula_fixed, . ~ . + (1 | family/genus/species_binom)),
  data = aus_data,
  REML = FALSE
)

library(MuMIn)
# Get marginal (fixed effects only) and conditional (fixed + random) R²
r.squaredGLMM(mixed_mod)

# Compare AICs
AIC(ols_mod, mixed_mod)


#thats a good story: taxonomy relatively improves fit 
#but not significantly so 
#to get imporved fit


mixed_sp <- lmer(
  update(formula_fixed, . ~ . + (1|species_binom)),
  data = aus_data,
  REML = FALSE
)
#better than no random effect, slightly worse than taxonomic random effect
AIC(ols_mod, mixed_mod, mixed_sp)
