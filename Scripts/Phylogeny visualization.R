install.packages("ape")
install.packages("phytools")
install.packages("nlme")
install.packages("plotrix")
install.packages("tidytree")

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
library("tidytree")

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


################## Data Tidying ###################
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
#replaced non existing concentrations with 0 

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

avg_nutrient_df <- aggregate(. ~ species_binom, data = nutrient_df, FUN = mean)


#use rownames and reorder 
rownames(avg_nutrient_df) <- avg_nutrient_df$species_binom
avg_nutrient_df <- avg_nutrient_df[match(ITS_tree$tip.label, rownames(avg_nutrient_df)), ]
#replace row names, and set NA values to 0 
avg_nutrient_df$species_binom[51] <- "Eucalyptus_trivalva" #most recent naming
avg_nutrient_df$species_binom[106] <- "Ginkgo_biloba"
avg_nutrient_df <- avg_nutrient_df %>%
  mutate_all(~replace(., is.na(.), 0))
rownames(avg_nutrient_df) <- avg_nutrient_df$species_binom
avg_nutrient_df <- avg_nutrient_df %>%
  rename(label = species_binom) 

avg_nutrient_tib <- as_tibble(avg_nutrient_df) 

ITS_tree_nutrient_tib <- left_join(ITS_tree_tib,
                  avg_nutrient_tib, by = "label") 

write.csv(ITS_tree_nutrient_tib, "ITS_tree_nutrient_data.csv",
          row.names = FALSE)

################## Tree Nutrient Plots ###################
#nutrient concentrations
dotTree(ITS_tree, as.matrix(avg_nutrient_df)[,c("leaf_N_per_dry_mass")])
#kinda useless



#covariance 


