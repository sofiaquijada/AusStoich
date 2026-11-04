library(ape)
library(tidyverse)
library(MCMCglmm)
library(coda)
library(ggtree)
library(dplyr)
library(ggplot2)
library(corrplot)

#need to do this to install ggtree
install.packages("BiocManager")
library(BiocManager)
BiocManager::install("ggtree")

#this script will host analyses with phylogenetic random effect variance structure
#will not be using categorical variables as predictors
#will not be using complete resolution tree
#SP will be logged since residuals are skewed from env_trait diagnostics
#---- double check this with an old MCMC object (expect to see skewness)

#-------------------------------Read in Data------------------------------------

#set working directory as needed
setwd("")

#read in trait data
aus_data <- read_csv(file = "Inputs/aus_data2026.csv")

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

#inverse wishart prior for phylogeny
prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 1), #for phylo
    G2 = list(V = 1, nu = 1)  #for species
  ),
  R = list(V = 1, nu = 1) #residual 
)
#should be weak prior ---- i think this is what i ended up with

Nnitt =  210000
Nburnin = 20000
Nthin = 50 #try thinning more ??? 

#specify path and trait choice
setwd("")

#following loop will run analyses for all traits + outputs and diagnostics ---

basepath <- "Results/MCMCglmm"

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
    if (i == 3){
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
  }
}

#-------------------------------Diagnostics-------------------------------------

#set wd to onedrive where mods stored

#set this in session
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Fiona Soper, Dr - Sofia AusStoich Project/Summer 2026")

plot_residuals <- function(model, response, model_name = "", plot_type) {
  # Get posterior predictive means of the data
  # If you know some linear algebra, you can see the this
  # is like fitting each point to the average value of your
  # posterior distribution for each parameter.
  predicted <- model$X %*% colMeans(model$Sol)
  
  # The residuals are simply the observed values minus
  # the predicted values
  residuals <- response - predicted
  
  if (plot_type == "residual_vs_observed") {
    #residual vs observed plot: 
    # Plot residuals vs fitted values
    # too small of a deviance from the observed values
    # are indicative of overfitting.
    plot(predicted, residuals,
         main = paste("Residuals", model_name),
    )
    abline(h = 0, lty = 2, col = "grey40")
    
  }
  
  if (plot_type == "expected_vs_observed"){
    #expected vs observed plot:
    # To get at the comfort zone/confidence in fitting 
    plot(response, predicted,
         main = paste("Residuals", model_name))
  }
}

plot_residuals(chain1_10, aus_data$ln_leaf_N, "ln leaf N",
               plot_type = "expected_vs_observed")

#define chain
chain1 <- ln_CP_ratio_chain1
chain2 <- ln_CP_ratio_chain2
chain3 <- ln_CP_ratio_chain3

#plot residuals: assess whether flattened at 0
plot_residuals(chain1, aus_data$ln_CP_ratio, "C:P")

#trace plots
plot(chain3)

#--plots for pre-existing model objects:

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
gelman.diag(combined_chains_VCV)
gelman.diag(combined_chains_sol)


#---------------------------------Results---------------------------------------

#need to var part from combined chain objects both sol and VCV
x <- get_info(chain)
x$prop #returns MCMC sample matrix with the proportions, need to take mean of samples

#this function will partition variance from combined chain objects
#and output a tidy dataframe ready for plotting
#essentially the same as get info
combined_chains_sol <- mcmc(as.matrix(mcmc.list(chain1$Sol, chain2$Sol, chain3$Sol)))
combined_chains_VCV <- mcmc(as.matrix(mcmc.list(chain1$VCV, chain2$VCV, chain3$VCV)))

var_part <- function(chain1, chain2, chain3, prec = 4){ 
  #prec = precision parameter
  #Extract the matrix containing posterior samples of the variance components
  #create combined chain objects to use all possible information
  
  #create mcmc list, turn into matrix to merge results
  combined_chains_sol <- mcmc(as.matrix(mcmc.list(chain1$Sol, chain2$Sol, chain3$Sol)))
  combined_chains_VCV <- mcmc(as.matrix(mcmc.list(chain1$VCV, chain2$VCV, chain3$VCV)))
  
  VCV <- combined_chains_VCV
  
  # Extract individual columns of the VCV matrix
  # Note: the "units" columns refers to the residual variance
  var_phylo <- VCV[,"phylo"]
  var_species <- VCV[,"species_binom"]
  var_resid <- VCV[,"units"]
  
  # Add the random components of variance
  var_random <- var_phylo + var_species

  #multiply fixed design matrix with transposed model solutions
  # = fitted values at each iteration for fixed effects
  #design matrix will be the same no matter what the chain, so just pick the first
  Xbeta <- chain1$X %*% t(combined_chains_sol)
  
  #compute variance over each column of fitted values using apply
  #to get variance in fitted values across observations 
  var_fixed <- apply(Xbeta, 2, FUN = var)
  
  # Get the total variance, this is defined as the sum of all the components
  var_tot <- var_fixed + var_phylo + var_species + var_resid
  
  # Calculate the marginal R2
  # This is the variance attributable only to the fixed effects variables
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

extract_var <- function(chain1, chain2, chain3, trait){
  
  res <- var_part(chain1, chain2, chain3)
  
  #take mean variance proportions
  means <- colMeans(res$prop)
  
  data.frame(
    foliar_trait = trait,
    group = c("environment", "phylogeny", "species", "residual"),
    prop = means
  )
}
#test:
var_part(chain1, chain2, chain3)
a <- extract_var(chain1, chain2, chain3 , "C:P")

#read in individually, then rm() mods since they are too heavy
N_var <- extract_var(ln_leaf_N_chain1, ln_leaf_N_chain2, ln_leaf_N_chain3,  "N")
P_var <- extract_var(ln_leaf_P_chain1, ln_leaf_P_chain2, ln_leaf_P_chain3,  "P")
C_var <- extract_var(leaf_C_per_dry_mass_chain1, leaf_C_per_dry_mass_chain2, leaf_C_per_dry_mass_chain3, "C")
NP_var <- extract_var(ln_NP_ratio_chain1, ln_NP_ratio_chain2, ln_NP_ratio_chain3, "N:P")
CN_var <- extract_var(ln_CN_ratio_chain1,ln_CN_ratio_chain2, ln_CN_ratio_chain3, "C:N")
CP_var <- extract_var(ln_CP_ratio_chain1, ln_CP_ratio_chain2, ln_CP_ratio_chain3, "C:P")

#prepare for plotting
#variance partitioning plot
#try to theme it exactly like LMMs
total_parts <- bind_rows(
  N_var,
  P_var,
  C_var,
  NP_var,
  CN_var,
  CP_var
  )

#written into onedrive as to not repeat this process:
#write_csv(total_parts, file = "varpart_combined_chains.csv")

total_parts$group <- factor(
  total_parts$group,
  levels = c("residual", "environment", "species", "phylogeny"))

total_parts$foliar_trait <- factor(total_parts$foliar_trait,levels = c("N","P","C","NP","CN","CP"))

ggplot(total_parts,
       aes(x = foliar_trait,
           y = prop,
           fill = group)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Foliar chemistry",
    y = "Proportion of total variance",
    title = "Variance Partition in trait ~ Phylogeny + Species + Environment"
  ) +
  scale_fill_manual(
    values = c(
      phylogeny = "#0099CC",
      species = "#7570B3",
      environment = "#D95F02",
      residual = "#1B9E77"
    ),
    breaks = c("phylogeny", "species", "environment", "residual"),
    labels = c("Phylogeny", "Species", "Environment", "Residual")
  ) +
  scale_x_discrete(limits = rev(levels(factor(total_parts$foliar_trait)))) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 20),
    axis.text.y = element_text(size = 15),   
    axis.text.x = element_text(size = 15),
    legend.text = element_text(size = 0),
    legend.title = element_text(size = 0),
    plot.title = element_text(size = 20)
  )

library(ggpubr)
ggpubr::as_ggplot(get_legend(p))

ggsave("Outputs/Visualizations/Manuscript/mcmc_varpart_legend.png", dpi = 800)

#read in and combine results:
traits <- c("ln_leaf_N", "ln_leaf_P", "leaf_C_per_dry_mass",
            "ln_NP_ratio", "ln_CN_ratio", "ln_CP_ratio")

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

for (trait in traits) {
  #read the 3 chains
  chains <- lapply(1:3, function(i) {
    file <- file.path(trait, paste0(trait, "_chain", i, "_Sol.csv"))
    
    data <- read_csv(file)
    
    #remove first column, convert to mcmc
    data <- data %>% select(-1)
    as.mcmc(data)
  })
  #create mcmc list
  combined_list <- mcmc.list(chains)
  
  #convert to mcmc object
  combined_mcmc <- mcmc(as.matrix(combined_list))
  #save combined mcmc object
  assign(paste0(trait, "_combined"), combined_mcmc)
  
  rm(chains, combined_mcmc)
}

#function to extract coefficient estimates from combined chain object
get_coefs_combined <- function(combined_mcmc_obj) {
  df <- data.frame(
    post.mean = colMeans(combined_mcmc_obj), #fixed effect output
    l_95_CI = HPDinterval(combined_mcmc_obj)[, "lower"], #HPD interval lower
    u_95_CI = HPDinterval(combined_mcmc_obj)[, "upper"], #HPD interval upper 
    eff.samp = effectiveSize(combined_mcmc_obj) #effective sample size
  )
  
  df$term <- rownames(df) #add term column
  
  df <- as_tibble(df)
}

N_coefs <- get_coefs_combined(ln_leaf_N_combined)
N_coefs <- N_coefs %>% mutate(trait = "ln_leaf_N")

P_coefs <- get_coefs_combined(ln_leaf_P_combined)
P_coefs <- P_coefs %>% mutate(trait = "ln_leaf_P")

C_coefs <- get_coefs_combined(leaf_C_per_dry_mass_combined)
C_coefs <- C_coefs %>% mutate(trait = "leaf_C_per_dry_mass")

NP_coefs <- get_coefs_combined(ln_NP_ratio_combined)
NP_coefs <- NP_coefs %>% mutate(trait = "ln_NP_ratio")

CN_coefs <- get_coefs_combined(ln_CN_ratio_combined)
CN_coefs <- CN_coefs %>% mutate(trait = "ln_CN_ratio")

CP_coefs <- get_coefs_combined(ln_CP_ratio_combined)
CP_coefs <- CP_coefs %>% mutate(trait = "ln_CP_ratio")

fixed_estimates_combined <- bind_rows(N_coefs, P_coefs, C_coefs,
                                      NP_coefs, CN_coefs, CP_coefs)
#write_csv(fixed_estimates_combined, file = "fixed_estimates_combined.csv")

#plot estimates
plot_coefs_mcmc <- function(coef_df, trait_name, pred_order){
  #remove intercept column, set predictor order
  coef_df <- coef_df %>%
  filter(term != "(Intercept)") %>% 
  mutate(term = factor(term, levels = pred_order)) #set order
  
  coef_df$term <- factor(coef_df$term,
                         levels = predictor_order)
  
  ggplot(coef_df,
         aes(x = post.mean,
             y = term)) +
    geom_vline(xintercept = 0,
               linetype = "dashed") +
    geom_errorbarh(aes(xmin = l_95_CI,
                       xmax = u_95_CI),
                   height = 0) +
    geom_point(size = 3) +
    scale_y_discrete(labels = term_labels) +
    labs(
      x = "Posterior mean",
      y = "",
      title = trait_name
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      plot.title = element_text(size = 20), 
      axis.title = element_text(size = 20)
    )
}

plot_coefs_mcmc(N_coefs, "Leaf N - MCMC", predictor_order)
plot_coefs_mcmc(P_coefs, "Leaf P - MCMC", predictor_order)
plot_coefs_mcmc(C_coefs, "Leaf C - MCMC", predictor_order)
plot_coefs_mcmc(NP_coefs, "Leaf NP - MCMC", predictor_order)
plot_coefs_mcmc(CN_coefs, "Leaf CN - MCMC", predictor_order)
plot_coefs_mcmc(CP_coefs, "Leaf CP - MCMC", predictor_order)


library(tidyr)
library(patchwork)

add_missing_predictors <- function(df, predictor_order) {
  df %>%
    complete(
      term = predictor_order,
      fill = list(
        estimate = NA_real_,
        lower = NA_real_,
        upper = NA_real_
      )
    ) %>%
    mutate(term = factor(term, levels = predictor_order))
}

N_coefs <- add_missing_predictors(N_coefs, predictor_order)
P_coefs <- add_missing_predictors(P_coefs, predictor_order)
C_coefs <- add_missing_predictors(C_coefs, predictor_order)
NP_coefs <- add_missing_predictors(NP_coefs, predictor_order)
CN_coefs <- add_missing_predictors(CN_coefs, predictor_order)
CP_coefs <- add_missing_predictors(CP_coefs, predictor_order)


plot_coef_missing_preds <- function(df, pred_order, title, show_y = TRUE) {
  
  #remove intercept column, set predictor order
  df <- df %>%
    filter(term != "(Intercept)") %>% 
    mutate(term = factor(term, levels = pred_order)) #set order
  
  df$term <- factor(df$term,
                         levels = predictor_order)
  
  ggplot(df,
         aes(x = post.mean,
             y = term)) +
    
    geom_vline(xintercept = 0,
               linetype = "dashed") +
    
    geom_errorbarh(aes(xmin = l_95_CI,
                       xmax = u_95_CI),
                   height = 0,
                   na.rm = TRUE) +
    
    geom_point(size = 3,
               na.rm = TRUE) +
    
    scale_y_discrete(
      labels = if (show_y) term_labels else NULL,
      drop = FALSE
    ) +
    
    labs(
      x = "Posterior mean",
      y = NULL,
      title = title
    ) +
    
    theme_minimal() +
    
    theme(
      axis.text.y =
        if (show_y)
          element_text(size = 13)
      else
        element_blank(),
      axis.ticks.y =
        if (show_y)
          element_line()
      else
        element_blank(),
        #axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 20), 
        axis.title = element_text(size = 20)
    )
}

plot_coef_missing_preds(N_coefs, predictor_order, "Leaf N", TRUE)

p1 <- plot_coef_missing_preds(N_coefs, predictor_order, "Leaf N", TRUE)
p2 <- plot_coef_missing_preds(P_coefs, predictor_order, "Leaf P", FALSE)
p3 <- plot_coef_missing_preds(C_coefs, predictor_order, "Leaf C", FALSE)

p4 <- plot_coef_missing_preds(NP_coefs, predictor_order, "Leaf NP", TRUE)
p5 <- plot_coef_missing_preds(CN_coefs, predictor_order, "Leaf CN", FALSE)
p6 <- plot_coef_missing_preds(CP_coefs, predictor_order, "Leaf CP", FALSE)

p1 + p2 + p3
p4 + p5 + p6
