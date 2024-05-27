install.packages("ape")
install.packages("phytools")
install.packages("nlme")
library(ape)
library(phytools)
library(nlme)
library(visreg)
library(dplyr)


ITS_tree <- read.nexus("ITS_tree.tre")
ITS_tree_species <- ITS_tree$tip.label 

plot(ITS_tree, cex = 0.3) #for some reason DQ191445 instead of gingko biloba

#need to ensure species names are EXACTLY (case sensitive) 
#in the data frame & in the tree

#fixing DQ191445 entry
print(ITS_tree_species[length(ITS_tree_species)]) #last entry
ITS_tree_species[length(ITS_tree_species)] <- "Ginkgo_biloba"
ITS_tree$tip.label <- ITS_tree_species
plot(ITS_tree, cex = 0.3)

#flipping so matches data daniel sent
#do this later... can't seem to figure out. but have correct data

#add underscore for spaces between family and genus
ITS_tree_species <- gsub(" ", "_", ITS_tree_species)
ITS_tree$tip.label <- ITS_tree_species
plot(ITS_tree, cex = 0.3)

austraits_leaf_stoich <- read.csv("austraits_leaf_stoichiometry_MASTER_for_phylogeny.csv")


filtered_df <- austraits_leaf_stoich[austraits_leaf_stoich$species_binom %in%
                                       ITS_tree_species, ] #fine
num_species <- filtered_df %>% 
  summarize(num_species = n_distinct(species_binom)) #104, should be 106...
missing_species <- ITS_tree_species[!(ITS_tree_species %in% 
                                        austraits_leaf_stoich$species_binom)]
#in the tree but not in our data: ginkgo bilboa,eucalyptus trivalvis 


################## Tree plots with Average Nutrient Values ###################
nutrient_df <-filtered_df[, c("species_binom", "leaf_N_per_dry_mass",
                              "leaf_P_per_dry_mass", "leaf_C_per_dry_mass")]

#replace NA with 0 so it can calculate means and COV
nutrient_df[, c("leaf_N_per_dry_mass", "leaf_P_per_dry_mass", "leaf_C_per_dry_mass")] <- 
  lapply(nutrient_df[, c("leaf_N_per_dry_mass", "leaf_P_per_dry_mass", 
                         "leaf_C_per_dry_mass")], 
         function(x) replace(x, is.na(x), 0))
# rownames(nutrient_df) <- nutrient_df$species_binom. duplicates not allowed

a <- nutrient_df %>% 
  summarize(num_species = n_distinct(species_binom)) #104 so ok 

avg_nutrient_df <- aggregate(. ~ species_binom, data = nutrient_df, FUN = mean)

#use rownames and reorder 
rownames(avg_nutrient_df) <- avg_nutrient_df$species_binom
avg_nutrient_df <- avg_nutrient_df[match(ITS_tree$tip.label, rownames(avg_nutrient_df)), ]
#missing species in df shown as NA rows


dotTree(ITS_tree, as.matrix(avg_nutrient_df)[,c("leaf_N_per_dry_mass")]) # plot trait "x" at tree tips

dotTree(ITS_tree, as.matrix(avg_nutrient_df)[, "leaf_P_per_dry_mass"])













################## Tree plots with Covariance ###################
#create covariance columns 