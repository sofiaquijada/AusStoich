library(ape)
library(caper)
library(dplyr)
library(tibble)
library(here)
library(ggtree)
library(broom)
library(tidyr)
library(httpgd)
library(nlme)

hgd()
hgd_browse()

#note to self 
#works, but extremely messy. rewrite when possible
#code for ratio at the end can serve as template



# PLGS regressions
# Need tree and associated subset of aus_data, follow steps.
# Following code is for entire aus_data

#1. Read in tree + remove internal node labels.

phylo_tree <- read.tree(here("Inputs/Trees/ausdata.tre"))


# To inspect internal nodes:
View(as.data.frame(phylo_tree$node.label))

#Note: w/o removal of internal node labels, "tips and labels" won't
#match when creating comparative data object
phylo_tree$node.label <- NULL
print(phylo_tree$Nnode) 
View(as.data.frame(phylo_tree$node.label))
View(phylo_tree) #Node num should be > 0


# 2. Get average trait data with associated variation metrics matched with tree information.

#Prune to cleaner selection, only include nutrient and env.
# TO-DO!!!

#Add variation metrics, then get average per species.
#Force to dataframe as rownames can't be set to tibbles
avg_var_ausdata <- average_nutrient_data(add_variation(aus_data))
avg_var_ausdata <- as.data.frame(avg_var_ausdata)
row.names(avg_var_ausdata) <- avg_var_ausdata$species_binom

#Match order between tree tip labels and nutrient data.
ausdata_match <- avg_var_ausdata[match(phylo_tree$tip.label, row.names(avg_var_ausdata)), ]

#Want comparable estimates for env/climate predictors
#Z-score them using base R scale()

# List of predictor columns to be z-scored
predictor_columns <- c("SN_total_0_30", "SP_total_0_30", "SOC_total_0_30",
                       "CEC_total_0_30", "AP_total_0_30", "NPP", "MAT", "PPT", "AET",
                       "precipitation_seasonality", "temp_seasonality")

# Z-score the predictor columns
ausdata_match[predictor_columns] <- scale(ausdata_match[predictor_columns])

library(corrplot)
corrplot(cor(data[predictor_columns]))

# 3. Create a comparative data object.

comp_data <- comparative.data(phy = phylo_tree, data = ausdata_match, names.col = "species_binom")


# 4. Perform PGLS

#Simple phylo linear regression
pgls_model <- pgls(log(avg_leaf_N) ~ SN_total_0_30, data = comp_data, lambda = "ML")
summary(pgls_model)


#Multiple linear regression

pgls(log(avg_leaf_N) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                       CEC_total_0_30 + AP_total_0_30 + NPP + MAT + PPT + AET +
                       precipitation_seasonality + temp_seasonality,
                       data = comp_data,
                       lambda = "ML")

#remove colinear variables
#temp_seasonality, AET, NPP 
#copied from variable selection in modeling script, will properly replicate
#to do! later...
N_pgls_model <- pgls(log(avg_leaf_N) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
                precipitation_seasonality,
                   data = comp_data,
                   lambda = "ML")
summary(N_pgls_model)
plot(N_pgls_model)
abline(a = 0, b = 1, col = "red", lty = 2)

P_pgls_model <- pgls(log(avg_leaf_P) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                       CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
                       precipitation_seasonality,
                     data = comp_data,
                     lambda = "ML")
summary(P_pgls_model)
plot(P_pgls_model)
abline(a = 0, b = 1, col = "red", lty = 2)


C_pgls_model <- pgls(log(avg_leaf_C) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                       CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
                       precipitation_seasonality,
                     data = comp_data,
                     lambda = "ML")
summary(C_pgls_model)
plot(C_pgls_model)
abline(a = 0, b = 1, col = "red", lty = 2)


#Attempting to plot estimates

# Function to extract coefficients and statistics from a pgls model
extract_pgls <- function(model, model_name) {
  summary_model <- summary(model)
  coefs <- summary_model$coefficients
  data.frame(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    statistic = coefs[, "t value"],
    p.value = coefs[, "Pr(>|t|)"],
    model = model_name,
    type = "PGLS"
  )
}

# Extract coefficients and statistics from each model
N_tidy <- extract_pgls(N_pgls_model, "N")
P_tidy <- extract_pgls(P_pgls_model, "P")
C_tidy <- extract_pgls(C_pgls_model, "C")

# Combine the extracted data into a single data frame
combined_tidy <- bind_rows(N_tidy, P_tidy, C_tidy)
#to parse through significant estimates or not
combined_tidy <- combined_tidy %>%
  mutate(significant = ifelse(p.value <= 0.05, "Significant", "Not Significant"))


# Plot all estimates
  ggplot(combined_tidy, aes(x = model, y = estimate, color = model)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of PGLS Models for log(N), log(P), and log(C)",
       x = "Model",
       y = "Estimate") +
  theme(legend.position = "right")


# Plot estimates by significance
  ggplot(combined_tidy, aes(x = model, y = estimate, color = model, shape = significant)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) +
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of PGLS Models for log(N), log(P), and log(C), p < 0.05",
       x = "Model",
       y = "Estimate",
       color = "Model",
       shape = "Significance") +
  theme(legend.position = "right")



# --------------- Now do same model without phylogeny: GLS

#GLS allows for errors that are correlated and not normally distributed
#following code is essentially the same as an OLS though

N_gls <- gls(log(avg_leaf_N) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
               CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
               precipitation_seasonality, ausdata_match)

summary(N_gls)
plot(N_gls)

P_gls <- gls(log(avg_leaf_P) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
               CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
               precipitation_seasonality, ausdata_match,
                na.action = na.omit)
summary(P_gls)
plot(P_gls)

C_gls <- gls(log(avg_leaf_C) ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
               CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
               precipitation_seasonality, ausdata_match,
               na.action = na.omit)
summary(C_reg)
plot(C_gls)

#tidy() can't get info from gls data
extract_gls <- function(model, model_name) {
  summary_model <- summary(model)
  coefs <- summary_model$tTable
  data.frame(
    term = rownames(coefs),
    estimate = coefs[, "Value"],
    std.error = coefs[, "Std.Error"],
    statistic = coefs[, "t-value"],
    p.value = coefs[, "p-value"],
    model = model_name,
    type = "GLS"
  )
}

#extract model outputs
N_tidy_gls <- extract_gls(N_gls, "N")
P_tidy_gls <- extract_gls(P_gls, "P")
C_tidy_gls <- extract_gls(C_gls, "C")

combined_gls <- bind_rows(N_tidy_gls, P_tidy_gls, C_tidy_gls)

combined_gls <- combined_gls %>%
  mutate(significant = ifelse(p.value <= 0.05, "Significant", "Not Significant"))

combined <-rbind(combined_tidy, combined_gls)

combined <- combined %>%
  mutate(term = case_match(
    term,
    "(Intercept)" ~ "Intercept",
    "SN_total_0_30" ~ "Total Soil Nitrogen",
    "SP_total_0_30" ~ "Total Soil Phosphorus",
    "SOC_total_0_30" ~ "Total Soil Carbon",
    "CEC_total_0_30" ~ "Cation Exchange Capacity", 
    "AP_total_0_30" ~ "Available Phosphorus",
    "MAT" ~ "Mean Annual Temperature",
    "PPT" ~ "Mean Annual Precipitation",
    "precipitation_seasonality" ~ "Precipitation Seasonality",
    .default = term  #NA unless this is included
  ))

#only one
ggplot(combined_gls, aes(x = model, y = estimate, color = model, shape = significant)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) + 
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of GLS Models for log(N), log(P), and log(C), p < 0.05",
       x = "Model",
       y = "Estimate",
       color = "Model",
       shape = "Significance") +
  theme(legend.position = "right")


#check AIC of models
AIC_gls <- AIC(C_gls)
AIC_pgls <- AIC(C_pgls_model)


AIC_gls 
AIC_pgls 


ggplot(combined, aes(x = model, y = estimate,
                     shape = type,
                     fill = model,
                     color = model,
                     alpha = significant)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("GLS" = 22, "PGLS" = 24)) +
  scale_alpha_manual(values = c("Significant" = 1, "Not Significant" = 0.4)) +
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of GLS and PGLS Models for log(N), log(P), and log(C), p < 0.05",
       x = "Model",
       y = "Estimate",
       shape = "Model Type",
       fill = "Model",
       color = "Model",
       alpha = "Significance") +
  theme(legend.position = "right")


ggplot(combined, aes(x = model, y = estimate,
                            shape = type,
                            fill = model,
                            color = model,
                            alpha = significant)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("GLS" = 22, "PGLS" = 24)) +
  scale_alpha_manual(values = c("Significant" = 1, "Not Significant" = 0.4)) +
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of GLS and PGLS Models for log(N), log(P), and log(C), p < 0.05",
       x = "Model",
       y = "Estimate",
       shape = "Model Type",
       fill = "Model",
       color = "Model",
       alpha = "Significance") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, color = "black"),  # Title size and color
    strip.text = element_text(size = 12, color = "black"),  # Facet labels size and color
    axis.title = element_text(size = 14, color = "black"),  # Axis titles size and color
    axis.text = element_text(size = 9, color = "black"),  # Axis text size and color
  )


#same, but with ratios -----------------

#look at skewness first: linear fit validation

ggplot(data = ausdata_match, mapping = aes(x = avg_geo_CN_ratio)) +
  geom_histogram() +
  theme_minimal() +
  ggplot(data = ausdata_match, mapping = aes(x = avg_geo_CN_ratio)) +
  geom_histogram() +
  theme_minimal() +
  ggplot(data = ausdata_match, mapping = aes(x = avg_geo_CP_ratio)) +
  geom_histogram() +
  theme_minimal()
#right skewed, but normal enough

A <- pgls(avg_geo_NP_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                       CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
                       precipitation_seasonality,
                     data = comp_data,
                     lambda = "ML")
summary(A)

B <- pgls(avg_geo_CN_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
            CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
            precipitation_seasonality,
          data = comp_data,
          lambda = "ML")
summary(B)

C <- pgls(avg_geo_CP_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
            CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
            precipitation_seasonality,
          data = comp_data,
          lambda = "ML")
summary(C)

ratio_estimates <- bind_rows(extract_pgls(A, "N:P"),
                             extract_pgls(B, "C:N"),
                             extract_pgls(C, "C:P")) %>%
  mutate(significant = ifelse(p.value <= 0.05, "Significant", "Not Significant"))

#OLS
D <- gls(avg_geo_NP_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
               CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
               precipitation_seasonality, ausdata_match, na.action = na.omit)
summary(D)

E <- gls(avg_geo_CN_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
           CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
           precipitation_seasonality, ausdata_match, na.action = na.omit)
summary(E)

G <- gls(avg_geo_CP_ratio ~ SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
           CEC_total_0_30 + AP_total_0_30 + MAT + PPT +
           precipitation_seasonality, ausdata_match, na.action = na.omit)
summary(G)


#combine estimates


ratio_estimates <- rbind(ratio_estimates,
  bind_rows(extract_gls(D, "N:P"), extract_gls(E, "C:N"),extract_gls(G, "C:P"))  %>%
  mutate(significant = ifelse(p.value <= 0.05, "Significant", "Not Significant"))) 

ratio_estimates <- ratio_estimates %>%
  mutate(term = case_match(
    term,
    "(Intercept)" ~ "Intercept",
    "SN_total_0_30" ~ "Total Soil Nitrogen",
    "SP_total_0_30" ~ "Total Soil Phosphorus",
    "SOC_total_0_30" ~ "Total Soil Carbon",
    "CEC_total_0_30" ~ "Cation Exchange Capacity", 
    "AP_total_0_30" ~ "Available Phosphorus",
    "MAT" ~ "Mean Annual Temperature",
    "PPT" ~ "Mean Annual Precipitation",
    "precipitation_seasonality" ~ "Precipitation Seasonality",
    .default = term  #NA unless this is included
  ))

         
#AIC
AIC(C) #pols
AIC(G) #ols

#plot
ggplot(ratio_estimates, aes(x = model, y = estimate,
                            shape = type,
                            fill = model,
                            color = model,
                            alpha = significant)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c("GLS" = 22, "PGLS" = 24)) +
  scale_alpha_manual(values = c("Significant" = 1, "Not Significant" = 0.4)) +
  facet_wrap(~ term, scales = "free_y") +
  theme_minimal() +
  labs(title = "Estimates of GLS and PGLS Models for N:P, C:N, and C:P ratios, p < 0.05",
       x = "Model",
       y = "Estimate",
       shape = "Model Type",
       fill = "Model",
       color = "Model",
       alpha = "Significance") +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, color = "black"),  # Title size and color
    strip.text = element_text(size = 12, color = "black"),  # Facet labels size and color
    axis.title = element_text(size = 14, color = "black"),  # Axis titles size and color
    axis.text = element_text(size = 9, color = "black"),  # Axis text size and color
  )
