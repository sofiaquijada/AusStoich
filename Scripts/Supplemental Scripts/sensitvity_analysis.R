library(ape)
library(tidyverse)
library(MCMCglmm)
library(coda)
library(ggtree)
library(tictoc)
library(dplyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(car)

#amine email sept 10
#try nu = 0.002, then 1, then 5 then 10 
#look for changes in the narrowness of the posterior credible intervals 

#data import -------------------------------------------------------------------

#set working directory as needed
#setwd()
#using remote

#--read in trait data
aus_data <- read_csv("Remote/Data/aus_data.csv")

#scale continuous predictors for comparable estimates
cont_predictors <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

cat_predictors <- c("myc_type", "woodiness", "putative_BNF", "reclass_life_history")

predictors <- c(cont_predictors, cat_predictors)

aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

#rewrite categorical variables as binary
#AM, EcM, EcM-AM, ErM, NM, NM-AM, split combined types into two 
#final columns will be AM, EcM, ErM, and NM

#process:
#pivot wider including hybrid types
#then split hybrid types into Am, EcM, and NM using conditionals
aus_data <- aus_data %>%
  mutate(
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

#--read in tree data
ausdata_tree <- read.tree("Remote/Data/ausdata.tre")

#sanity check, inverseA requires ultrametric trees
is.ultrametric(ausdata_tree) #TRUE

#look at node labels
table(ausdata_tree$node.label)
#remove internal node labels ""
ausdata_tree$node.label <- NULL 

#inverted phylogenetic covariance matrix
phyloinv_complete <- inverseA(ausdata_tree, nodes ="TIPS",scale=TRUE) 
#tree with some uncertain nodes


#--prep for MCMCglmm
#get column for phylo tip labels, and another for species
aus_data$phylo <- aus_data$species_binom


#ensure columns are factors
aus_data$phylo <- factor(aus_data$phylo)
aus_data$species_binom <- factor(aus_data$species_binom)

#family column causes issues
aus_data <- aus_data %>% rename(fam = family)
aus_data$family <- NULL 

#MCMCglmm requires dataframe
#or else will say: some levels of phylo do not have a row entry in ginverse
all(levels(aus_data$phylo) %in% rownames(phyloinv_complete$Ainv)) #TRUE

aus_data<- as.data.frame(aus_data)


#variable selection ------------------------------------------------------------
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
#all VIFs below or marginally above 10 once AET removed
aus_data$AET <- NULL

#assess colinearity of categorical and continuous predictors
lm_check <- lm(ln_leaf_N ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 + CEC_total_0_30 +
                 AP_total_0_30 + NPP + MAT + PPT + precipitation_seasonality +
                 temp_seasonality + putative_BNF + myc_type + woodiness,
               data = aus_data)
vif(lm_check)
#VIF for categories are < 5, so probably ok!
#model.matrix function for checking correlation b/w categorical and continuous vars

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
  geom_histogram(mapping = aes(x = (ln_CP_ratio))) +
  theme_minimal()

#MCMCglmm-----------------------------------------------------------------------

#try nu = 0.002, 1, 5, 10
#look for changes in posterior credible interval of variables

#sensitivity analysis
prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 10), #for phylo
    G2 = list(V = 1, nu = 10)  #for species
  ),
  R = list(V = 1, nu = 10) #residual 
)

Nnitt = 10000
Nburnin = 1000
Nthin = 20

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich")
filepath <- "Results/Sensitivity Analysis/nu = 10"
response <- "ln_leaf_N"

#following loop will run three chains for desired trait + outputs & diagnostics
for (i in 1:3) {
  
  model_formula <- as.formula(
    paste0(response, " ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 + ",
           "CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + ",
           "precipitation_seasonality + temp_seasonality")
  )
  
  chain <- MCMCglmm(
    model_formula,
    random = ~ phylo + species_binom,
    family = "gaussian",
    ginverse = list(phylo = phyloinv_complete$Ainv),
    prior = prior_phylo,
    data = aus_data,
    nitt = Nnitt, burnin = Nburnin, thin = Nthin
  )
  
  #save text summary for ease of access
  sink(file.path(filepath, paste0(response, "_chain", i, "_summary.txt")))
  print(summary(chain))
  sink()
  
  #save chain diagnostics
  sink(file.path(filepath, paste0(response, "_chain", i, "_heidel.txt")))
  print(heidel.diag(chain$Sol))
  print(heidel.diag(chain$VCV))
  sink()
  
  #save variance partitioning
  sink(file.path(filepath, paste0(response, "_chain", i, "_varpart.txt")))
  print(get_info(chain))
  sink()
  
  #save chain solutions and model
  saveRDS(chain, file = file.path(filepath, paste0(response, "_chain", i, ".RDS")))
  write.csv(chain$Sol, file = file.path(filepath, paste0(response, "_chain", i, "_Sol.csv")))
  write.csv(chain$VCV, file = file.path(filepath, paste0(response, "_chain", i, "_VCV.csv")))
  
  #store chain
  assign(paste0("chain", i), chain)
  
  count = 0 + i
  
  #once 3 chains have run, create combined chain object
  #save gelman.diag + HPD intervals
  if (count == 3){
    combined_chains_sol <- mcmc.list(chain1$Sol, chain2$Sol, chain3$Sol)
    combined_chains_VCV <- mcmc.list(chain1$VCV, chain2$VCV, chain3$VCV)
    
    sink(file.path(filepath, paste0(response, "_gelman.diag_sol.txt")))
    print(gelman.diag(combined_chains_sol))
    sink()
    
    sink(file.path(filepath, paste0(response, "_gelman_diag_VCV.txt")))
    print(gelman.diag(combined_chains_VCV))
    sink()
    
    sink(file.path(filepath, paste0(response, "HPD_interval_sol.txt")))
    print(HPDinterval(combined_chains_sol))
    sink()
    
    sink(file.path(filepath, paste0(response, "HPD_interval_VCV.txt")))
    print(HPDinterval(combined_chains_VCV))
    sink()
  }
}
#--------------------
#following loop will run analyses for all traits + outputs and diagnostics ---
traits <- c("ln_NP_ratio", "ln_CN_ratio", "ln_CP_ratio","ln_leaf_P", "leaf_C_per_dry_mass")

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich")
base_path <- "Results/Sensitivity Analysis/nu = 0.002"


for (response in traits) {
  
  #create output folder
  filepath <- file.path(base_path, response)
  dir.create(filepath, recursive = TRUE, showWarnings = FALSE)
  
  #remove potential leftover chains
  rm(list = c("chain1", "chain2", "chain3", "chain"), envir = .GlobalEnv)
  
  for (i in 1:3) {
    
    model_formula <- as.formula(
      paste0(response, " ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 + ",
             "CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + ",
             "precipitation_seasonality + temp_seasonality")
    )
    
    chain <- MCMCglmm(
      model_formula,
      random = ~ phylo + species_binom,
      family = "gaussian",
      ginverse = list(phylo = phyloinv_complete$Ainv),
      prior = prior_phylo,
      data = aus_data,
      nitt = Nnitt, burnin = Nburnin, thin = Nthin
    )
    
    #save text summary for ease of access
    sink(file.path(filepath, paste0(response, "_chain", i, "_summary.txt")))
    print(summary(chain))
    sink()
    
    #save chain diagnostics
    sink(file.path(filepath, paste0(response, "_chain", i, "_heidel.txt")))
    print(heidel.diag(chain$Sol))
    print(heidel.diag(chain$VCV))
    sink()
    
    #save variance partitioning
    sink(file.path(filepath, paste0(response, "_chain", i, "_varpart.txt")))
    print(get_info(chain))
    sink()
    
    #save chain solutions and model
    saveRDS(chain, file = file.path(filepath, paste0(response, "_chain", i, ".RDS")))
    write.csv(chain$Sol, file = file.path(filepath, paste0(response, "_chain", i, "_Sol.csv")))
    write.csv(chain$VCV, file = file.path(filepath, paste0(response, "_chain", i, "_VCV.csv")))
    
    #store chain
    assign(paste0("chain", i), chain)
    
    #once 3 chains have run, create combined chain object
    #save gelman.diag + HPD intervals
    if (count == 3){
      combined_chains_sol <- mcmc.list(chain1$Sol, chain2$Sol, chain3$Sol)
      combined_chains_VCV <- mcmc.list(chain1$VCV, chain2$VCV, chain3$VCV)
      
      sink(file.path(filepath, paste0(response, "_gelman.diag_sol.txt")))
      print(gelman.diag(combined_chains_sol))
      sink()
      
      sink(file.path(filepath, paste0(response, "_gelman_diag_VCV.txt")))
      print(gelman.diag(combined_chains_VCV))
      sink()
      
      sink(file.path(filepath, paste0(response, "HPD_interval_sol.txt")))
      print(HPDinterval(combined_chains_sol))
      sink()
      
      sink(file.path(filepath, paste0(response, "HPD_interval_VCV.txt")))
      print(HPDinterval(combined_chains_VCV))
      sink()
    }
    rm(chain1, chain2, chain3, chain)
    gc() #garbage collection
  }
}