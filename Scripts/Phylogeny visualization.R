install.packages("ape")
install.packages("phytools")
install.packages("nlme")
install.packages("plotrix")
install.packages("tidytree")
install.packages("dplyr")

install.packages("BiocManager")
BiocManager::install("ggtree")
library(ape)
library(phytools)
library(nlme)
library(visreg)
library(dplyr)
library(plotrix)
library(ggtree)
library(ggplot2)
library(tidytree)
library(ggtree)
library(treeio)
library(tibble)
library(tidyverse)
library(cowplot)



#---------------- Function Definitions ----------------




#------------------------------------------------------

ITS_tree <- read.nexus("Inputs/ITS_tree.tre")
ITS_tree_tib <- as_tibble(ITS_tree) #NA columns = internal nodes

ITS_tree_species <- ITS_tree$tip.label

plot(ITS_tree, cex = 0.3) #for some reason DQ191445 instead of gingko biloba

#need to ensure species names are EXACTLY (case sensitive) 
#in the data frame & in the tree

#fixing DQ191445 entry
print(ITS_tree_species[length(ITS_tree_species)]) #last entry
ITS_tree_species[length(ITS_tree_species)] <- "Ginkgo_biloba"
ITS_tree$tip.label <- ITS_tree_species
ITS_tree_tib <- as_tibble(ITS_tree) #update tib with proper name
plot(ITS_tree, cex = 0.3)

#add underscore for spaces between family and genus
ITS_tree_species <- gsub(" ", "_", ITS_tree_species)
ITS_tree$tip.label <- ITS_tree_species
plot(ITS_tree, cex = 0.3)

austraits_leaf_stoich <- read.csv("Inputs/austraits_leaf_stoichiometry_MASTER_for_phylogeny.csv")


filtered_df <- austraits_leaf_stoich[austraits_leaf_stoich$species_binom %in%
                                       ITS_tree_species, ] #fine

num_species <- filtered_df %>% 
  summarize(num_species = n_distinct(species_binom)) #105
missing_species <- ITS_tree_species[!(ITS_tree_species %in% 
                                        austraits_leaf_stoich$species_binom)]
#in the tree but not in our data: Ginkgo_bilboa, outgroup


#---------------- Data Tidying ----------------
#function to calculate coefficient of variation
calc_cv <- function(x) {
  cv <- sd(x) / mean(x)
  return(cv)
} #currently not using

nutrient_df <-filtered_df[, c("species_binom", "leaf_N_per_dry_mass",
                              "leaf_P_per_dry_mass", "leaf_C_per_dry_mass")]

#replace NA with 0 so it can calculate means and COV
nutrient_df[, c("leaf_N_per_dry_mass", "leaf_P_per_dry_mass", "leaf_C_per_dry_mass")] <- 
  lapply(nutrient_df[, c("leaf_N_per_dry_mass", "leaf_P_per_dry_mass", 
                         "leaf_C_per_dry_mass")], 
         function(x) replace(x, is.na(x), 0))


nutrient_df <- nutrient_df %>%
  group_by(species_binom) %>%
  mutate(CV_N = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                             na.rm = TRUE),
         CV_P = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                             na.rm = TRUE),
         CV_C = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                             na.rm = TRUE))
length(unique(nutrient_df$species_binom)) #105 so ok 

#to properly aggregate, will replace all NaN arising from dividing by 0 to 0 
#a CV of 0 could mean the mean is 0, or the standard deviation is 0
nutrient_df <- mutate_all(nutrient_df, ~ ifelse(is.nan(.), 0, .))
#count samples
nutrient_df <- nutrient_df %>%
  add_count(species_binom, name = "frequency")

avg_nutrient_df <- aggregate(. ~ species_binom, data = nutrient_df, FUN = mean)

#use rownames and reorder 
rownames(avg_nutrient_df) <- avg_nutrient_df$species_binom
avg_nutrient_df <- avg_nutrient_df[match(ITS_tree$tip.label, rownames(avg_nutrient_df)), ]
#replace row names, and set NA values to 0 
#avg_nutrient_df$species_binom[51] <- "Eucalyptus_trivalvis" #most recent naming
#avg_nutrient_df$species_binom[106] <- "Ginkgo_biloba" #not here?
avg_nutrient_df <- avg_nutrient_df %>%
  mutate_all(~replace(., is.na(.), 0))
rownames(avg_nutrient_df) <- avg_nutrient_df$species_binom
avg_nutrient_df <- avg_nutrient_df %>%
  rename(label = species_binom) 

avg_nutrient_tib <- as_tibble(avg_nutrient_df) 

ITS_tree_nutrient_tib <- left_join(ITS_tree_tib,
                  avg_nutrient_tib, by = "label") 

write.csv(ITS_tree_nutrient_tib, "ITS_tree_nutrient_freq_data.csv",
          row.names = FALSE)

#---------------- Tree Nutrient Plots ----------------
ITS_tree_nutrients <- as.treedata(ITS_tree_nutrient_tib) #treedata object

#nutrient concentrations

dotTree(ITS_tree, as.matrix(avg_nutrient_df)[,c("leaf_N_per_dry_mass")])
#kinda useless

p <- ggtree(ITS_tree) + 
  geom_tiplab(size = 0.7)

tree_N_data_df <- avg_nutrient_df[,c("label", "leaf_N_per_dry_mass")]

facet_plot(p,'Leaf N', data =tree_N_data_df, geom = geom_col,
           mapping = aes(x=leaf_N_per_dry_mass),
           orientation = 'y', width = .5) #WORKS YEAHHHHHH

tree_C_data_df <- avg_nutrient_df[,c("label", "leaf_C_per_dry_mass")]
facet_plot(p,'Leaf C', data =tree_C_data_df, geom = geom_col,
           mapping = aes(x=leaf_C_per_dry_mass),
           orientation = 'y', width = .5) 

tree_P_CV_data_df <- avg_nutrient_df[,c("label", "CV_P")]
facet_plot(p,'Leaf P CV', data =tree_P_CV_data_df, geom = geom_col,
           mapping = aes(x=CV_P),
           orientation = 'y', width = .5) 

#look into "geom" parameters
#better labeling, add scale 
#fix name of trivalvis rownames
#make plots of covariance also, using geom_point
#note to self: ginkgo balboa lost for some reason... but its in csv so whatever 



#covariance 

#---------------- Avg Nutrient Plots ----------------

#remember these are averaged values
avg_N_conc <- ggplot(avg_nutrient_df, aes(x = label, y = leaf_N_per_dry_mass)) + 
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(title = "ITS tree species") +
  theme_minimal()

avg_P_conc <- ggplot(avg_nutrient_df, aes(x = label, y = leaf_P_per_dry_mass)) + 
  geom_bar(stat = "identity", fill = "yellow") +
  labs(title = "ITS tree species") +
  theme_minimal()

avg_C_conc <- ggplot(avg_nutrient_df, aes(x = label, y = leaf_C_per_dry_mass)) + 
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "ITS tree species") +
  theme_minimal()

avg_nut_plots <- plot_grid(
  avg_N_conc,
  avg_P_conc,
  avg_C_conc)

#plot by frequency, note outliers
ggplot(avg_nutrient_df, aes(x = frequency, y = leaf_N_per_dry_mass)) + 
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(title = "ITS tree species") +
  theme_minimal()

ggplot(avg_nutrient_df, aes(x = frequency, y = leaf_C_per_dry_mass)) + 
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "ITS tree species") +
  theme_minimal() 

ggplot(avg_nutrient_df, aes(x = frequency, y = leaf_P_per_dry_mass)) + 
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "ITS tree species") +
  theme_minimal() 

#transform data 
#transformed_avg_nutrient_df object
trns_avg_nut_df <- avg_nutrient_df %>% 
  mutate(
    ln_N = log(leaf_N_per_dry_mass),
    log_10_N = log10(leaf_N_per_dry_mass),
    sqrt_N = sqrt(leaf_N_per_dry_mass),
    ln_C = log(leaf_C_per_dry_mass),
    log_10_C = log10(leaf_C_per_dry_mass),
    sqrt_C = sqrt(leaf_C_per_dry_mass),
    ln_P = log(leaf_P_per_dry_mass),
    log_10_P = log10(leaf_P_per_dry_mass),
    sqrt_P = sqrt(leaf_P_per_dry_mass)
  )
#replace -inf with NA
trns_avg_nut_df[trns_avg_nut_df == "-Inf"] <- 0

#by all species
ggplot(trns_avg_nut_df, aes(x = label, y = ln_C)) + 
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "ITS tree species") +
  theme_minimal()

#by frequency
ggplot(trns_avg_nut_df, aes(x = ln_N, y = frequency)) + 
  geom_bar(stat = "identity") +
  labs(title = "ITS tree species") +
  theme_minimal() 

#maybe aggregate by frequency? so would get mean of many dif. species
#then it would make sense to normalize 
  