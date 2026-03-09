library(lme4)
library(tidyverse)
library(dplyr)
library(ggplot2)


#Fiona-requested analysis ------------------------------------------------------
#category + taxonomy = trait
#category + env = trait 

#then we can get an idea about whether category is a significant in these analyses
library(tidyverse)
library(corrplot)
#set working directory as needed
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich")

#---read in trait data
aus_data #from data import

#scale continuous predictors for comparable estimates
cont_predictors <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")
aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

#create binary myc_type categories
aus_data <- aus_data %>% mutate(
  AM     = as.integer(myc_type == "AM"),
  EcM    = as.integer(myc_type == "EcM"),
  `EcM-AM` = as.integer(myc_type == "EcM-AM"),
  ErM    = as.integer(myc_type == "ErM"),
  NM     = as.integer(myc_type == "NM"),
  `NM-AM`  = as.integer(myc_type == "NM-AM")
) %>%
  relocate(AM, EcM, `EcM-AM`, ErM, NM, `NM-AM`, .after = myc_type) %>%
  #add hybrid data to columns
  mutate(
    AM  = if_else(`EcM-AM` == 1 | `NM-AM` == 1, 1L, AM),
    EcM = if_else(`EcM-AM` == 1, 1L, EcM),
    NM  = if_else(`NM-AM` == 1, 1L, NM)
  ) %>% select(-`EcM-AM`, -`NM-AM`)

cat_predictors <- c("AM", "EcM", "ErM", "NM", "woodiness", "putative_BNF", "reclass_life_history")
predictors <- c(cont_predictors, cat_predictors)

#variable selection ------
env <- as.data.frame(aus_data[predictors])
env <- env %>% mutate(across(all_of(cat_predictors), as.character))

#compute VIF for continuous predictors
diag(solve(cor(env[cont_predictors])))

#plot correlated variables
corrplot(cor(aus_data[cont_predictors]))

#Highly colinear: AET-PPT, AET-temp_seasonality. Remove AET
env$AET <- NULL
cont_predictors <- cont_predictors[cont_predictors != "AET"]
diag(solve(cor(env[cont_predictors])))
corrplot(cor(aus_data[cont_predictors]))
#all VIFs below or marginally above 10 once AET removed
aus_data$AET <- NULL

#model matrix for colinearity between categorical and continuous variables
env_complete <- env[complete.cases(env), ]
env_complete[cat_predictors] <- lapply(env_complete[cat_predictors], as.factor)

X <- model.matrix(~ ., data = env_complete)
diag(solve( cor(X)))
C <- cor(X)
View(C) #cat variables as is seem to not be correlated with env 
#at least not linearly
#pearson n
cov(X)

#trait distributions to check normality assumption
ggplot(data = aus_data) +
  geom_histogram(mapping = aes(x = leaf_C_per_dry_mass)) +
  theme_minimal()

# Example: trait is your response, cont_predictors and cat_predictors are lists of column names
formula_fixed <- as.formula(
  paste("ln_NP_ratio ~", paste(c(cont_predictors, cat_predictors), collapse = " + "))
)

#category + taxonomy = trait
cat_tax_lm <- lmer(
  ln_NP_ratio ~ AM + EcM + ErM + NM + woodiness +
    putative_BNF + reclass_life_history +
    (1 | family/genus/species_binom),
  data = aus_data)
AIC(cat_tax_lm) #3672.646
summary(cat_tax_lm)

library(MuMIn)
r.squaredGLMM(cat_tax_lm) #gives marginal and conditional R2's

#category + env = trait
cat_env_lm <- lm(formula_fixed, data = aus_data)
summary(cat_env_lm)
AIC(cat_env_lm) #4906.499


#category + env + taxonomy = trait
cat_env_tax_lm <- lmer(
  update(formula_fixed, . ~ . + (1 | family/genus/species_binom)),
  data = aus_data) 
summary(cat_env_tax_lm)
AIC(cat_env_tax_lm) #3495.702

