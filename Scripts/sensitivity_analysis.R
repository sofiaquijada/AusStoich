library(ape)
library(tidyverse)
library(MCMCglmm)
library(coda)
library(ggtree)
library(dplyr)
library(ggplot2)
library(corrplot)

#amine email sept 10
#try nu = 0.002, then 1, then 5 then 10 
#look for changes in the narrowness of the posterior credible intervals 


#-------------------------------Read in Data------------------------------------

#set working directory as needed
setwd("")

#read in trait data
aus_data <- read_csv(file = "aus_data2026.csv")

#scale continuous predictors for comparable estimates
cont_predictors <- c("SN_total_0_30", "log_SP_total_0_30", "SOC_total_0_30",
                     "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                     "precipitation_seasonality", "temp_seasonality")

aus_data$log_SP_total_0_30 <- log(aus_data$SP_total_0_30)

#scale after logging SP
aus_data[cont_predictors] <- scale(aus_data[cont_predictors])

#--read in tree data
ausdata_tree <- read.tree("ausdata.tre")

#sanity check, inverseA requires ultrametric trees
is.ultrametric(ausdata_tree) #TRUE

#look at node labels
table(ausdata_tree$node.label)
#remove internal node labels ""
ausdata_tree$node.label <- NULL 

#inverted phylogenetic covariance matrix
phyloinv <- inverseA(ausdata_tree, nodes ="TIPS",scale=TRUE)

#-----------------------------Prep for MCMCglmm---------------------------------
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
all(levels(aus_data$phylo) %in% rownames(phyloinv$Ainv)) #TRUE

aus_data<- as.data.frame(aus_data)

#variable selection done in qmd file, outputs read in here
selected_variables <- readRDS("selected_variables.rds")

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

#-------------------------------VarPart function--------------------------------

#from Amine
get_info <- function(mod, prec = 4){
  # Extract the matrix containing posterior samples of the variance components
  VCV <- mod$VCV
  
  # Extract individual columns of the VCV matrix
  # Note: the "units" columns refers to the residual variance
  var_phylo <- VCV[,"phylo"]
  var_species <- VCV[,"species_binom"]
  var_resid <- VCV[,"units"]
  
  # Add the random components of variance
  # In our case, these are the phylo and species component
  var_random <- var_phylo + var_species
  
  # Calculate the predicted values for each observation and each MCMC iteration
  # These are the predicted values across MCMC samples
  # xbeta = bayesian analog to theta hat in conventional statistics
  
  #multiply fixed design matrix with transposed model solutions
  # = fitted values at each iteration for fixed effects
  Xbeta <- mod$X %*% t(mod$Sol)
  
  # You can get the fixed component of variance from these values 
  # variance in fitted values across observations 
  var_fixed <- apply(Xbeta, 2, var)
  
  # Get the total variance, this is defined as the sum of all the components
  # in the model
  var_tot <- var_fixed + var_phylo + var_species + var_resid
  
  # Calculate the marginal R2
  # This is the variance attributable only to the fixed effects variables
  # Therefore, it makes sense to define it as the ratio:
  R2_marginal <- var_fixed / var_tot
  print(paste0("Mean marginal R2: ", round(mean(R2_marginal), prec)))
  
  # Calculate the conditional R2
  # This is the variance attributable to the fixed effects variables, while
  # also considering the random effects variables
  # Therefore, it makes sense to define it as the ratio:
  R2_conditional <- (var_fixed + var_random) / var_tot
  print(paste0("Mean conditional R2: ", round(mean(R2_conditional), prec)))
  
  # You can also compute the proportion of total variance attributed to each
  # component, rather than as the two R2 values.
  # Note: You'll notice that prop_fixed == R2_marginal
  prop_fixed <- var_fixed / var_tot
  prop_phylo <- var_phylo / var_tot
  prop_species <- var_species / var_tot
  prop_resid <- var_resid / var_tot
  
  # Posterior means of each
  print(paste0("Mean fixed_p: ", round(mean(prop_fixed), prec)))
  print(paste0("Mean phylo_p: ", round(mean(prop_phylo), prec)))
  print(paste0("Mean species_p: ", round(mean(prop_species), prec)))
  print(paste0("Mean resid_p: ", round(mean(prop_resid), prec)))
  
  # Return results
  invisible(list(
    R2 = cbind(R2_marginal, R2_conditional),
    prop = cbind(prop_fixed, prop_phylo, prop_species, prop_resid)
  ))
}

#-------------------------------MCMCglmm----------------------------------------

#try nu = 0.002, then 1, then 5 then 10 

NU = 0.002

#inverse wishart prior for phylogeny
prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = NU), #for phylo
    G2 = list(V = 1, nu = NU)  #for species
  ),
  R = list(V = 1, nu = NU) #residual 
)

Nnitt = 10000
Nburnin = 1000
Nthin = 20

#specify path and trait choice
basepath <- "Results/Sensitivity Analysis/nu = 0.002"

#following loop will run analyses for all traits + outputs and diagnostics ---


for (response in traits) {
  
  #create output folder
  filepath <- file.path(basepath, response)
  dir.create(filepath, recursive = TRUE, showWarnings = FALSE)
  
  #remove potential leftover chains
  rm(list = c("chain1", "chain2", "chain3", "chain"), envir = .GlobalEnv)
  
  #read in selected (linearly indep) variables
  current_cont_predictors <- selected_variables[[response]]
  
  for (i in 1:3) {
    
    model_formula <- as.formula(
      paste0(response, "~", paste(c(current_cont_predictors),
                                  collapse = " + "))
    )
    
    chain <- MCMCglmm(
      model_formula,
      random = ~ phylo + species_binom,
      family = "gaussian",
      ginverse = list(phylo = phyloinv$Ainv),
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

#-------------------------------------------------------------------------------
#look for changes in the narrowness of the posterior credible intervals 

#plot estimates with their posterior credible intervals
#on their combined chain objects

term_labels <- c(
  AET = "Actual Evapotranspiration",
  temp_seasonality = "Temperature Seasonality",
  precipitation_seasonality = "Precipitation Seasonality",
  PPT = "Precipitation",
  NPP = "Net Primary Productivity",
  MAT = "Mean Annual Temperature",
  CEC_total_0_30 = "Cation Exchange Capacity",
  AP_total_0_30 = "Available Phosphorus",
  log_SP_total_0_30 = "Total Soil Phosphorus (log)",
  SOC_total_0_30 = "Total Soil Carbon",
  SN_total_0_30 = "Total Soil Nitrogen"
)

predictor_order <- c(
  "temp_seasonality",
  "precipitation_seasonality",
  "PPT",
  "NPP",
  "MAT",
  "CEC_total_0_30",
  "AP_total_0_30",
  "log_SP_total_0_30",
  "SOC_total_0_30",
  "SN_total_0_30"
)

#function to extract coefficient estimates from combined chain object
get_nu_coefs <- function(combined_mcmc_obj, nu_val, pred_order) {
  df <- data.frame(
    post.mean = colMeans(combined_mcmc_obj), #fixed effect output
    l_95_CI = HPDinterval(combined_mcmc_obj)[, "lower"], #HPD interval lower
    u_95_CI = HPDinterval(combined_mcmc_obj)[, "upper"], #HPD interval upper 
    eff.samp = effectiveSize(combined_mcmc_obj) #effective sample size
  )
  
  df$term <- rownames(df) #add term column
  df$nu <- as.factor(nu_val) #add nu column
  
  df %>% #remove intercept columns
    filter(term != "(Intercept)") %>% 
    mutate(term = factor(term, levels = pred_order)) #set order
}

#------------------------------leaf N

#---- nu = 0.002
N_0.002_combined <- mcmc(as.matrix(mcmc.list(N1_0.002$Sol, N2_0.002$Sol, N3_0.002$Sol)))
N_0.002_coef <- get_nu_coefs(N_0.002_combined, 0.002, predictor_order)

#---- nu = 1
N_1_combined <- mcmc(as.matrix(mcmc.list(N1_1$Sol, N2_1$Sol, N3_1$Sol)))
N_1_coef <- get_nu_coefs(N_1_combined, 1, predictor_order)

#---- nu = 5
N_5_combined <- mcmc(as.matrix(mcmc.list(N1_5$Sol, N2_5$Sol, N3_5$Sol)))
N_5_coef <- get_nu_coefs(N_5_combined, 5, predictor_order)

#---- nu = 10
N_10_combined <- mcmc(as.matrix(mcmc.list(N1_10$Sol, N2_10$Sol, N3_10$Sol)))
N_10_coef <- get_nu_coefs(N_10_combined, 10, predictor_order)

#---- combine nu's 
N_all_coefs <- bind_rows(N_0.002_coef, N_1_coef, N_5_coef, N_10_coef)

#---- update the plot
ggplot(N_all_coefs,
       aes(x = post.mean,
           y = term,
           color = nu)) + # Color points and bars by nu
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_errorbarh(aes(xmin = l_95_CI,
                     xmax = u_95_CI),
                 height = 0.3, # Added small height so overlapping bars are visible
                 position = position_dodge(width = 1)) + # Dodges bars so they don't overlap vertically
  geom_point(size = 3,
             position = position_dodge(width = 1)) + # Dodges points to match the error bars
  scale_y_discrete(labels = term_labels) +
  labs(
    x = "Posterior mean",
    y = "",
    title = "ln leaf N",
    color = expression(nu) # Formats legend title as the Greek letter nu
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13)
  )

rm(N_0.002_coef, N_1_coef, N_10_coef, N_5_coef, N_all_coefs,
   N1_0.002, N1_1, N1_10, N1_5, N2_0.002, N2_1, N2_10, N2_5,
   N3_0.002, N3_1, N3_10, N3_5, N_0.002_combined, N_1_combined,
   N_5_combined, N_10_combined)

#------------------------------leaf P
#---- nu = 0.002
P_0.002_combined <- mcmc(as.matrix(mcmc.list(P1_0.002$Sol, P2_0.002$Sol, P3_0.002$Sol)))
P_0.002_coef <- get_nu_coefs(P_0.002_combined, 0.002, predictor_order)

#---- nu = 1
P_1_combined <- mcmc(as.matrix(mcmc.list(P1_1$Sol, P2_1$Sol, P3_1$Sol)))
P_1_coef <- get_nu_coefs(P_1_combined, 1, predictor_order)

#---- nu = 5
P_5_combined <- mcmc(as.matrix(mcmc.list(P1_5$Sol, P2_5$Sol, P3_5$Sol)))
P_5_coef <- get_nu_coefs(P_5_combined, 5, predictor_order)

#---- nu = 10
P_10_combined <- mcmc(as.matrix(mcmc.list(P1_10$Sol, P2_10$Sol, P3_10$Sol)))
P_10_coef <- get_nu_coefs(P_10_combined, 10, predictor_order)

#---- combine nu's 
P_all_coefs <- bind_rows(P_0.002_coef, P_1_coef, P_5_coef, P_10_coef)

#---- update the plot
ggplot(P_all_coefs,
       aes(x = post.mean,
           y = term,
           color = nu)) + # Color points and bars by nu
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_errorbarh(aes(xmin = l_95_CI,
                     xmax = u_95_CI),
                 height = 0.3, # Added small height so overlapping bars are visible
                 position = position_dodge(width = 1)) + # Dodges bars so they don't overlap vertically
  geom_point(size = 3,
             position = position_dodge(width = 1)) + # Dodges points to match the error bars
  scale_y_discrete(labels = term_labels) +
  labs(
    x = "Posterior mean",
    y = "",
    title = "ln leaf P",
    color = expression(nu) # Formats legend title as the Greek letter nu
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13)
  )

rm(P_0.002_coef, P_1_coef, P_10_coef, P_5_coef, P_all_coefs,
   P1_0.002, P1_1, P1_10, P1_5, P2_0.002, P2_1, P2_10, P2_5,
   P3_0.002, P3_1, P3_10, P3_5, P_0.002_combined, P_1_combined,
   P_5_combined, P_10_combined)

#------------------------------leaf C
#---- nu = 0.002
C_0.002_combined <- mcmc(as.matrix(mcmc.list(C1_0.002$Sol, C2_0.002$Sol, C3_0.002$Sol)))
C_0.002_coef <- get_nu_coefs(C_0.002_combined, 0.002, predictor_order)

#---- nu = 1
C_1_combined <- mcmc(as.matrix(mcmc.list(C1_1$Sol, C2_1$Sol, C3_1$Sol)))
C_1_coef <- get_nu_coefs(C_1_combined, 1, predictor_order)

#---- nu = 5
C_5_combined <- mcmc(as.matrix(mcmc.list(C1_5$Sol, C2_5$Sol, C3_5$Sol)))
C_5_coef <- get_nu_coefs(C_5_combined, 5, predictor_order)

#---- nu = 10
C_10_combined <- mcmc(as.matrix(mcmc.list(C1_10$Sol, C2_10$Sol, C3_10$Sol)))
C_10_coef <- get_nu_coefs(C_10_combined, 10, predictor_order)

#---- combine nu's 
C_all_coefs <- bind_rows(C_0.002_coef, C_1_coef, C_5_coef, C_10_coef)

#---- update the plot
ggplot(C_all_coefs,
       aes(x = post.mean,
           y = term,
           color = nu)) + # Color points and bars by nu
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_errorbarh(aes(xmin = l_95_CI,
                     xmax = u_95_CI),
                 height = 0.3, # Added small height so overlapping bars are visible
                 position = position_dodge(width = 1)) + # Dodges bars so they don't overlap vertically
  geom_point(size = 3,
             position = position_dodge(width = 1)) + # Dodges points to match the error bars
  scale_y_discrete(labels = term_labels) +
  labs(
    x = "Posterior mean",
    y = "",
    title = "leaf C",
    color = expression(nu) # Formats legend title as the Greek letter nu
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13)
  )

rm(C_0.002_coef, C_1_coef, C_10_coef, C_5_coef, C_all_coefs,
   C1_0.002, C1_1, C1_10, C1_5, C2_0.002, C2_1, C2_10, C2_5,
   C3_0.002, C3_1, C3_10, C3_5, C_0.002_combined, C_1_combined,
   C_5_combined, C_10_combined)

#------------------------------ln NP ratio
#---- nu = 0.002
NP_0.002_combined <- mcmc(as.matrix(mcmc.list(NP1_0.002$Sol, NP2_0.002$Sol, NP3_0.002$Sol)))
NP_0.002_coef <- get_nu_coefs(NP_0.002_combined, 0.002, predictor_order)

#---- nu = 1
NP_1_combined <- mcmc(as.matrix(mcmc.list(NP1_1$Sol, NP2_1$Sol, NP3_1$Sol)))
NP_1_coef <- get_nu_coefs(NP_1_combined, 1, predictor_order)

#---- nu = 5
NP_5_combined <- mcmc(as.matrix(mcmc.list(NP1_5$Sol, NP2_5$Sol, NP3_5$Sol)))
NP_5_coef <- get_nu_coefs(NP_5_combined, 5, predictor_order)

#---- nu = 10
NP_10_combined <- mcmc(as.matrix(mcmc.list(NP1_10$Sol, NP2_10$Sol, NP3_10$Sol)))
NP_10_coef <- get_nu_coefs(NP_10_combined, 10, predictor_order)

#---- combine nu's 
NP_all_coefs <- bind_rows(NP_0.002_coef, NP_1_coef, NP_5_coef, NP_10_coef)

#---- update the plot
ggplot(NP_all_coefs,
       aes(x = post.mean,
           y = term,
           color = nu)) + # Color points and bars by nu
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_errorbarh(aes(xmin = l_95_CI,
                     xmax = u_95_CI),
                 height = 0.3, # Added small height so overlapping bars are visible
                 position = position_dodge(width = 1)) + # Dodges bars so they don't overlap vertically
  geom_point(size = 3,
             position = position_dodge(width = 1)) + # Dodges points to match the error bars
  scale_y_discrete(labels = term_labels) +
  labs(
    x = "Posterior mean",
    y = "",
    title = "ln NP ratio",
    color = expression(nu) # Formats legend title as the Greek letter nu
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13)
  )

rm(NP_0.002_coef, NP_1_coef, NP_10_coef, NP_5_coef, NP_all_coefs,
   NP1_0.002, NP1_1, NP1_10, NP1_5, NP2_0.002, NP2_1, NP2_10, NP2_5,
   NP3_0.002, NP3_1, NP3_10, NP3_5, NP_0.002_combined, NP_1_combined,
   NP_5_combined, NP_10_combined)

#------------------------------ln CN ratio
#---- nu = 0.002
CN_0.002_combined <- mcmc(as.matrix(mcmc.list(CN1_0.002$Sol, CN2_0.002$Sol, CN3_0.002$Sol)))
CN_0.002_coef <- get_nu_coefs(CN_0.002_combined, 0.002, predictor_order)

#---- nu = 1
CN_1_combined <- mcmc(as.matrix(mcmc.list(CN1_1$Sol, CN2_1$Sol, CN3_1$Sol)))
CN_1_coef <- get_nu_coefs(CN_1_combined, 1, predictor_order)

#---- nu = 5
CN_5_combined <- mcmc(as.matrix(mcmc.list(CN1_5$Sol, CN2_5$Sol))) #3rd chain didnt work..
CN_5_coef <- get_nu_coefs(CN_5_combined, 5, predictor_order)

#---- nu = 10
CN_10_combined <- mcmc(as.matrix(mcmc.list(CN1_10$Sol, CN2_10$Sol, CN3_10$Sol)))
CN_10_coef <- get_nu_coefs(CN_10_combined, 10, predictor_order)

#---- combine nu's 
CN_all_coefs <- bind_rows(CN_0.002_coef, CN_1_coef, CN_5_coef, CN_10_coef)

#---- update the plot
ggplot(CN_all_coefs,
       aes(x = post.mean,
           y = term,
           color = nu)) + # Color points and bars by nu
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_errorbarh(aes(xmin = l_95_CI,
                     xmax = u_95_CI),
                 height = 0.3, # Added small height so overlapping bars are visible
                 position = position_dodge(width = 1)) + # Dodges bars so they don't overlap vertically
  geom_point(size = 3,
             position = position_dodge(width = 1)) + # Dodges points to match the error bars
  scale_y_discrete(labels = term_labels) +
  labs(
    x = "Posterior mean",
    y = "",
    title = "ln CN ratio",
    color = expression(nu) # Formats legend title as the Greek letter nu
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13)
  )

rm(CN_0.002_coef, CN_1_coef, CN_10_coef, CN_5_coef, CN_all_coefs,
   CN1_0.002, CN1_1, CN1_10, CN1_5, CN2_0.002, CN2_1, CN2_10, CN2_5,
   CN3_0.002, CN3_1, CN3_10, CN3_5, CN_0.002_combined, CN_1_combined,
   CN_5_combined, CN_10_combined)

#------------------------------ln CP ratio
#---- nu = 0.002
CP_0.002_combined <- mcmc(as.matrix(mcmc.list(CP1_0.002$Sol, CP2_0.002$Sol, CP3_0.002$Sol)))
CP_0.002_coef <- get_nu_coefs(CP_0.002_combined, 0.002, predictor_order)

#---- nu = 1
CP_1_combined <- mcmc(as.matrix(mcmc.list(CP1_1$Sol, CP2_1$Sol, CP3_1$Sol)))
CP_1_coef <- get_nu_coefs(CP_1_combined, 1, predictor_order)

#---- nu = 5
CP_5_combined <- mcmc(as.matrix(mcmc.list(CP1_5$Sol, CP2_5$Sol, CP3_5$Sol)))
CP_5_coef <- get_nu_coefs(CP_5_combined, 5, predictor_order)

#---- nu = 10
CP_10_combined <- mcmc(as.matrix(mcmc.list(CP1_10$Sol, CP2_10$Sol, CP3_10$Sol)))
CP_10_coef <- get_nu_coefs(CP_10_combined, 10, predictor_order)

#---- combine nu's 
CP_all_coefs <- bind_rows(CP_0.002_coef, CP_1_coef, CP_5_coef, CP_10_coef)

#---- update the plot
ggplot(CP_all_coefs,
       aes(x = post.mean,
           y = term,
           color = nu)) + # Color points and bars by nu
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_errorbarh(aes(xmin = l_95_CI,
                     xmax = u_95_CI),
                 height = 0.3, # Added small height so overlapping bars are visible
                 position = position_dodge(width = 1)) + # Dodges bars so they don't overlap vertically
  geom_point(size = 3,
             position = position_dodge(width = 1)) + # Dodges points to match the error bars
  scale_y_discrete(labels = term_labels) +
  labs(
    x = "Posterior mean",
    y = "",
    title = "ln CP ratio",
    color = expression(nu) # Formats legend title as the Greek letter nu
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(size = 15),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13)
  )

rm(CP_0.002_coef, CP_1_coef, CP_10_coef, CP_5_coef, CP_all_coefs,
   CP1_0.002, CP1_1, CP1_10, CP1_5, CP2_0.002, CP2_1, CP2_10, CP2_5,
   CP3_0.002, CP3_1, CP3_10, CP3_5, CP_0.002_combined, CP_1_combined,
   CP_5_combined, CP_10_combined)