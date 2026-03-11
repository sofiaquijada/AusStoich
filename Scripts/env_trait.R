library(lme4)
library(tidyverse)
library(corrplot)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(broom.mixed)
library(lmerTest) #to get p-values, Satterthwaite's method


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
traits <- c("ln_NP_ratio", "ln_CN_ratio", "ln_CP_ratio",
            "ln_leaf_N", "ln_leaf_P", "leaf_C_per_dry_mass")

basepath <- "Results/Mixed & OLS models"

model_types <- c("cat_tax", "cat_env", "cat_env_tax")

for (response in traits) {
  
  for (model_type in model_types) {
    
    #create folder
    filepath <- file.path(basepath, response, model_type)
    dir.create(filepath, recursive = TRUE, showWarnings = FALSE)
    
    #build formula depending on model
    if (model_type == "cat_tax") {
      
      form <- as.formula(paste(trait,
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



#evaluate fit of each (18 models)

#checks:
#homogeneity
plot(resid(cat_env_lm) ~ fitted(cat_env_lm))
#independence of residuals with each covariate
par() #number of predictors to have all in one plot
plot(resid(cat_env_lm) ~ model.frame(cat_env_lm)$SN_total_0_30)
#normality of residuals
hist(resid(cat_env_lm))

#---check if random effects are necessary
#create linear model without random effect, calculate its residuals
#plot residuals against levels of random factors
cat_env_lm_resid <- rstandard(cat_env_lm)
#depends on how many obs were used

mf <- model.frame(cat_env_lm)
rows_used <- as.numeric(rownames(mf))

res <- residuals(cat_env_lm)
family <- aus_data$family[rows_used]

boxplot(res ~ family,xlab = "family", ylab = "LM residuals")

#Plot estimates based on significance 
#use tidy output


