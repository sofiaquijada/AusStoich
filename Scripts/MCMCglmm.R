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
  geom_histogram(mapping = aes(x = log(leaf_C_per_dry_mass))) +
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


#MCMCglmm settings and priors---------------------------------------------------

#inverse wishart prior for phylogeny
prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 1), #for phylo
    G2 = list(V = 1, nu = 1)  #for species
  ),
  R = list(V = 1, nu = 1) #residual 
)


Nnitt = 100000
Nburnin = 1000
Nthin = 10



#MCMCglmm ----------------------------------------------------------------------
tic("chain1")
#chains
#first check burn in period (low number of iterations)
chain1 <- MCMCglmm(ln_NP_ratio ~  SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                     + CEC_total_0_30 + AP_total_0_30 +
                     + NPP + MAT + PPT +
                     + precipitation_seasonality + temp_seasonality +
                     + reclass_life_history + putative_BNF + myc_type + woodiness,
                   random = ~ phylo +species_binom,
                   family = "gaussian",
                   ginverse = list(phylo = phylo_inv$Ainv), prior = prior_phylo,
                   data = ausdata_all_pos_sp, nitt = Nnitt, burnin = Nburnin, thin = Nthin)
chain1_sol <- chain1$Sol
toc()

chain2 <- MCMCglmm(ln_NP_ratio ~  SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                     + CEC_total_0_30 + AP_total_0_30 +
                     + NPP + MAT + PPT +
                     + precipitation_seasonality + temp_seasonality +
                     + reclass_life_history + putative_BNF + myc_type + woodiness,
                   random = ~ phylo +species_binom,
                   family = "gaussian",
                   ginverse = list(phylo = phylo_inv$Ainv), prior = prior_phylo,
                   data = ausdata_all_pos_sp, nitt = Nnitt, burnin = Nburnin, thin = Nthin)
chain2_sol <- chain2$Sol

chain3 <- MCMCglmm(ln_NP_ratio ~  SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                     + CEC_total_0_30 + AP_total_0_30 +
                     + NPP + MAT + PPT +
                     + precipitation_seasonality + temp_seasonality +
                     + reclass_life_history + putative_BNF + myc_type + woodiness,
                   random = ~ phylo +species_binom,
                   family = "gaussian",
                   ginverse = list(phylo = phylo_inv$Ainv), prior = prior_phylo,
                   data = ausdata_all_pos_sp, nitt = Nnitt, burnin = Nburnin, thin = Nthin)
chain3_sol <- chain3$Sol

combined_chains <- mcmc.list(chain1$Sol, chain2$Sol, chain3$Sol)


#save model outputs: raw and summary
write.csv(chain1_sol, file = "Results/chain1_sol.csv")
write.csv(chain2_sol, file = "Results/chain2_sol.csv")
write.csv(chain3_sol, file = "Results/chain3_sol.csv")

saveRDS(chain1)
saveRDS(chain2)
saveRDS(chain3)


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
  a <- gelman.diag(mcmc_chain)
  b <- plot(mcmc_chain)
  plot(a)
  plot(b)
}

plot_mcmc(chain1)
plot_mcmc(chain2)
plot_mcmc(chain3)

plot_chains(combined_chains)

#need MCMCclass
BF <- BayesFactor(chain1, chain2)
print(BF)

#---- summary statistics
summary(model)
summary(model)$solutions
plot(model$Sol) #fixed effects
plot(model$VCV) #random effects
autocorr.plot(model$Sol)

# Check convergence
gelman.diag(combined_chains) #does anova on different chains
traceplot(combined_chains)
autocorr.plot(chain1$Sol)
autocorr.plot(chain1$VCV)