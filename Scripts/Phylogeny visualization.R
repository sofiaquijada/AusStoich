install.packages("ape")
install.packages("phytools")
install.packages("nlme")
install.packages("plotrix")

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
#in the tree but not in our data: Ginkgo_bilboa, Eucalyptus_trivalvis 


################## Tree plots with Average Nutrient Values ###################
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
length(unique(nutrient_df$species_binom)) #104 so ok 

#to properly aggregate, will replace all NaN arising from dividing by 0 to 0 
#a CV of 0 could mean the mean is 0, or the standard deviation is 0
nutrient_df <- mutate_all(nutrient_df, ~ ifelse(is.nan(.), 0, .))

avg_nutrient_df <- aggregate(. ~ species_binom, data = nutrient_df, FUN = mean)


#use rownames and reorder 
rownames(avg_nutrient_df) <- avg_nutrient_df$species_binom
avg_nutrient_df <- avg_nutrient_df[match(ITS_tree$tip.label, rownames(avg_nutrient_df)), ]
#missing species in df shown as NA rows
#idea: replace as "0"


dotTree(ITS_tree, as.matrix(avg_nutrient_df)[,c("leaf_N_per_dry_mass")])
#kinda useless



# Remove NA values from the tip labels
non_na_labels_N <- na.omit(as.character(avg_nutrient_df$leaf_N_per_dry_mass))
non_na_labels_N <- non_na_labels[!is.na(non_na_labels)]

#base plot
plot(ITS_tree, cex = 0.3, main = "Mean N")
# Add tip labels
tiplabels(non_na_labels_N, adj = c(1, 0), frame = "none", cex = 0.3)


non_na_labels_P <- na.omit(as.character(avg_nutrient_df$leaf_P_per_dry_mass))
non_na_labels_P <- non_na_labels[!is.na(non_na_labels)]
plot(ITS_tree, cex = 0.3, main = "Mean P")
# Add tip labels
tiplabels(non_na_labels, adj = c(1, 0), frame = "none", cex = 0.3)

#this is wrong,,,, idk where numbers are coming from, there is no difference
#note: labels were not labelled until NA values ommited somehow
#so maybe go back and set NA labels as 0 or smth 

#maybe do bar plot w phylogenetic tree to visualize nutrient concentration 
#in each species?

matched_labels <- avg_nutrient_df$species_binom %in% ITS_tree$tip.label

# Print the unmatched labels
print(avg_nutrient_df$species_binom[!matched_labels])

#p <- plot(ITS_tree, cex = 0.3)
#p2 <- facet_plot(p, panel="bar", data=avg_nutrient_df, geom=geom_segment, aes(x=species_binom), color='red3')





################## Tree plots with Covariance ###################
#create covariance columns: done in avg_nutrient_df