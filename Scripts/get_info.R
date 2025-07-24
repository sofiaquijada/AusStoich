get_info <- function(mod, prec = 4)
{
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
  Xbeta <- mod$X %*% t(mod$Sol)
  
  # You can get the fixed component of variance from these values 
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
