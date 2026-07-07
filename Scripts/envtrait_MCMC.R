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

#inverse wishart prior for phylogeny
prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 1), #for phylo
    G2 = list(V = 1, nu = 1)  #for species
  ),
  R = list(V = 1, nu = 1) #residual 
)
#should be weak prior ---- i think this is what i ended up with

Nnitt = 210000
Nburnin = 20000
Nthin = 50

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

#-------------------------------Diagnostics-------------------------------------

#set wd to onedrive where mods stored

#set this in session
setwd("~/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Fiona Soper, Dr - Sofia AusStoich Project/Summer 2026")

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

#define chain
chain1 <- ln_CN_ratio_chain1
chain2 <- ln_CN_ratio_chain2
chain3 <- ln_CN_ratio_chain3

#plot residuals: assess whether flattened at 0
plot_residuals(chain3, aus_data$ln_CN_ratio, "C:N")

#trace plots
plot(chain1)

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
#note:
x <- get_info(chain)
x$prop #returns MCMC sample matrix with the proportions, need to take mean of samples

#variance partitioning plot
#try to theme it exactly like LMMs

extract_var <- function(chain, trait){
  
  res <- get_info(chain)
  
  #take mean variance proportions
  means <- colMeans(res$prop)
  
  data.frame(
    foliar_trait = trait,
    group = c("environment", "phylogeny", "species", "residual"),
    prop = means
  )
}

#prepare for plotting
#ADDD RATIOS ONCE CP IS DONE RUNNING
total_parts <- bind_rows(
  extract_var(ln_leaf_N_chain1,  "N"),
  extract_var(ln_leaf_P_chain1,  "P"),
  extract_var(leaf_C_per_dry_mass_chain1,  "C"),
  extract_var(ln_NP_ratio_chain1, "N:P"),
  extract_var(ln_CN_ratio_chain1, "C:N")
  #extract_var(ln_CP_ratio_chain1)
  )

total_parts$group <- factor(
  total_parts$group,
  levels = c("residual", "environment", "species", "phylogeny"))

ggplot(total_parts,
       aes(x = foliar_trait,
           y = prop,
           fill = group)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Foliar chemistry",
    y = "Proportion of total variance",
    title = "Variance Partition in trait ~ Enviroment + Phylogeny + Species mod"
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
  scale_x_discrete(
    limits = rev(levels(factor(total_parts$foliar_trait))),
    labels = c("CN","NP","C","P","N")) + #modify to include CP after
  theme_minimal() +
  theme(
    axis.title = element_text(size = 0),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 16)
  )


#plot estimates
plot_coefs_mcmc <- function(model, trait_name){
  
  #fixed effects summary
  coef_df <- as.data.frame(summary(model)$solutions)
  coef_df$term <- rownames(coef_df)
  
  #remove intercept
  coef_df <- coef_df |>
    dplyr::filter(term != "(Intercept)") |>
    dplyr::rename(
      estimate = post.mean,
      lower = `l-95% CI`,
      upper = `u-95% CI`
    )
  
  #predictor labels
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
  
  coef_df$term <- factor(coef_df$term,
                         levels = predictor_order)
  
  ggplot(coef_df,
         aes(x = estimate,
             y = term)) +
    geom_vline(xintercept = 0,
               linetype = "dashed") +
    geom_errorbarh(aes(xmin = lower,
                       xmax = upper),
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
      axis.text.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      plot.title = element_text(size = 15)
    )
}

plot_coefs_mcmc(ln_leaf_N_chain1, "Leaf N - MCMC")
plot_coefs_mcmc(ln_leaf_P_chain1, "Leaf P - MCMC")
plot_coefs_mcmc(leaf_C_per_dry_mass_chain1, "Leaf C - MCMC")
plot_coefs_mcmc(ln_NP_ratio_chain1, "N:P ratio - MCMC")
plot_coefs_mcmc(ln_CN_ratio_chain1, "C:N ratio - MCMC")
plot_coefs_mcmc(ln_CP_ratio_chain1, "C:P ratio - MCMC")
