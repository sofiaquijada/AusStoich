library(ape)
library(tidyverse)
library(MCMCglmm)
library(coda)
library(ggtree)
library(tictoc)
library(dplyr)
library(ggplot2)
library(corrplot)

#data import -------------------------------------------------------------------

#set working directory as needed
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich/Remote")

#--read in trait data
aus_data <- read_csv("Data/aus_data.csv")

#scale continuous predictors for comparable estimates
cont_predictors <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

cat_predictors <- c("myc_type", "woodiness", "putative_BNF", "reclass_life_history")

predictors <- c(cont_predictors, cat_predictors)

aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

#--read in tree data
ausdata_all_pos_sp_tree <- read.tree("Data/ausdata_all_pos_sp.tre")

#sanity check, inverseA requires ultrametric trees
is.ultrametric(ausdata_all_pos_sp_tree) #TRUE

#look at node labels
table(ausdata_all_pos_sp_tree$node.label)
#remove internal node labels ""
ausdata_all_pos_sp_tree$node.label <- NULL 

#inverted phylogenetic covariance matrix
phylo_inv <- inverseA(ausdata_all_pos_sp_tree, nodes ="TIPS",scale=TRUE)
#may compare results with inverse matrix using nodes = ALL later


#--prep for MCMCglmm
#get column for phylo tip labels, and another for species
aus_data$phylo <- aus_data$species_binom

#prune aus_data to include exclusively species in tree of choice
ausdata_all_pos_sp <- aus_data %>% 
  filter(species_binom %in% ausdata_all_pos_sp_tree$tip.label)

#ensure columns are factors
ausdata_all_pos_sp$phylo <- factor(ausdata_all_pos_sp$phylo)
ausdata_all_pos_sp$species_binom <- factor(ausdata_all_pos_sp$species_binom)

#family column causes issues
ausdata_all_pos_sp <- ausdata_all_pos_sp %>%
  rename(fam = family)
ausdata_all_pos_sp$family <- NULL

#MCMCglmm requires dataframe
#or else will say: some levels of phylo do not have a row entry in ginverse
all(levels(ausdata_all_pos_sp$phylo) %in% rownames(phylo_inv$Ainv)) #TRUE
ausdata_all_pos_sp <- as.data.frame(ausdata_all_pos_sp)

#trait distributions to check normality assumption
ggplot(data = aus_data) +
  geom_histogram(mapping = aes(x = log(leaf_P_per_dry_mass))) +
  theme_minimal()

#MCMCglmm can't have any NAs in fixed predictors
ausdata_all_pos_sp <- ausdata_all_pos_sp %>%
  filter(!is.na(myc_type) & !is.na(woodiness))


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
ausdata_all_pos_sp$AET <- NULL


#MCMCglmm-----------=-----------------------------------------------------------


#inverse wishart prior for phylogeny
prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 1), #for phylo
    G2 = list(V = 1, nu = 1)  #for species
  ),
  R = list(V = 1, nu = 1) #residual 
)


Nnitt = 210000
Nburnin = 20000
Nthin = 50


#specify path and trait choice
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/McGill/Soper Lab/AusStoich")
filepath <- "Results/GIC white/Attempt 2 NP ratio"
response <- "ln_NP_ratio"


#following loop will run three chains for desired trait + outputs & diagnostics
for (i in 1:3) {
  
  model_formula <- as.formula(
    paste0(response, " ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 + ",
           "CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + ",
           "precipitation_seasonality + temp_seasonality + ",
           "reclass_life_history + putative_BNF + myc_type + woodiness")
  )
  
  chain <- MCMCglmm(
    model_formula,
    random = ~ phylo + species_binom,
    family = "gaussian",
    ginverse = list(phylo = phylo_inv$Ainv),
    prior = prior_phylo,
    data = ausdata_all_pos_sp,
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



#model check -------------------------------------------------------------------

plot_residuals <- function(model, response, model_name = "") {
  # Get posterior predictive means of the data
  # If you know some linear algebra, you can see the this
  # is like fitting each point to the average value of your
  # posterior distribution for each parameter.
  predicted <- model$X %*% colMeans(model$Sol)
  
  # The residuals are simply the observed values minus
  # the predicted values
  residuals <- response - predicted
  
  # Plot residuals vs fitted values
  # too small of a deviance from the observed values
  # are indicative of overfitting.
  plot(predicted, residuals,
       main = paste("Residuals", model_name),
      )
  abline(h = 0, lty = 2, col = "grey40")
}

plot_residuals(chain3, ausdata_all_pos_sp$ln_CN_ratio, "CN")
#model diagnostics -------------------------------------------------------------

plot_mcmc <- function(mcmc_obj){
  #function for diagnostic plots for MCMCglmm objects
  p <- plot.MCMCglmm(mcmc_obj)
  a <- autocorr.plot(mcmc_obj$Sol)
  b <- autocorr.plot(mcmc_obj$VCV)
  plot(p)
  plot(a)
  plot(b)
}

plot_chains <- function(mcmc_chain){
  a <- gelman.plot(mcmc_chain)
  b <- plot(mcmc_chain)
  plot(a)
  plot(b)
}

plot_mcmc(chain1)
plot_mcmc(chain2)
plot_mcmc(chain3)

#code to get gelman.diag stats for those whose RDS wasn't saved properly:
#properly use as.mcmc for this

#some csv's saved rownames as column "1", remove if needed
chain1_sol <- subset(chain1_sol, select = -c(1))
chain2_sol <- subset(chain2_sol, select = -c(1))
chain3_sol <- subset(chain3_sol, select = -c(1))

#turn into mcmc objects
sol1 <- as.mcmc(x =  chain1_sol, start = 1, end = 3800, thin = 50)
sol2 <- as.mcmc(x =  chain2_sol, start = 1, end = 3800, thin = 50)
sol3 <- as.mcmc(x =  chain3_sol, start = 1, end = 3800, thin = 50)

chain1_VCV <- subset(chain1_VCV, select = -c(1))
chain2_VCV <- subset(chain2_VCV, select = -c(1))
chain3_VCV <- subset(chain3_VCV, select = -c(1))

VCV1 <- as.mcmc(x =  chain1_VCV, start = 1, end = 3800, thin = 50)
VCV2 <- as.mcmc(x =  chain2_VCV, start = 1, end = 3800, thin = 50)
VCV3 <- as.mcmc(x =  chain3_VCV, start = 1, end = 3800, thin = 50)

combined_chain_sol <- mcmc.list(sol1, sol2, sol3)
combined_chain_VCV <- mcmc.list(VCV1, VCV2, VCV3)

gelman.plot(combined_chain_VCV)
gelman.plot(combined_chain_sol)

#calculate HPD intervals
HPDinterval(combined_chain_sol)
HPDinterval(combined_chain_VCV)



#plots for pre-existing model objects:

combined_chains_sol <- mcmc.list(chain1$Sol, chain2$Sol, chain3$Sol)
combined_chains_VCV <- mcmc.list(chain1$VCV, chain2$VCV, chain3$VCV)

autocorr.plot(combined_chains_sol)
autocorr.plot(combined_chains_VCV)

geweke.plot(combined_chains_sol)
geweke.plot(combined_chains_VCV)

heidel.diag(combined_chains_sol)
heidel.diag(combined_chains_VCV)

gelman.plot(combined_chains_VCV)
gelman.plot(combined_chains_sol)

#---- summary statistics examples
summary(model)
summary(model)$solutions
plot(model$Sol) #fixed effects
plot(model$VCV) #random effects
autocorr.plot(model$Sol)
autocorr.plot(model$VCV)

# Check convergence
gelman.diag(combined_chains) #does anova on different chains
traceplot(combined_chains)
autocorr.plot(chain1$Sol)
autocorr.plot(chain1$VCV)


#----coda diagnostics
effectiveSize(chain1_sol) #can do this with VCV as well
heidel.diag(chain1_sol)
raftery.diag(chain1_sol)
geweke.diag