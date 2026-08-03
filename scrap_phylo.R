library(devtools) #to install packages from github
devtools::install_github("jinyizju/V.PhyloMaker2")
library(tidyverse)
library(ape)
library(ggtree)
library(tidytree)
library(treeio)
library(phytools)
library(V.PhyloMaker2)
library(httpgd)
library(here)
library(readr)

aus_data

httpgd::hgd() #VS code plot viewer
hgd_browse()



#this script has scrap code used for phylogenetic signal
#trying to clean up main phylosig script to get everything needed in a clean way
#so trees that weren't used will go here, functions that weren't used maybe
#goal is for phylosig script to stand on its own with the required data only
#no need for functions script since its messy
#and ausdata csv already has variation columns anyway

# all pos sp data entry ----
#this is for tree with total resolved species, no uncertain nodes
ausdata_all_pos_sp_tree <- read.tree("Inputs/Trees/ausdata_all_pos_sp.tre")
ausdata_all_pos_sp_df <- read_csv('Inputs/all_pos_austraits_LCVP_sp.csv')

all_pos_sp_data <- aus_data[aus_data$species_binom %in%
                              ausdata_all_pos_sp_df$species, ]



#all_pos_sp_data <- add_CV_columns(select_relevant_columns(all_pos_sp_data))
avg_all_pos_sp_data <- average_nutrient_data(select_relevant_columns(all_pos_sp_data))


ausdata_all_pos_sp_tree_tib <- as_tibble(ausdata_all_pos_sp_tree)
ausdata_all_pos_sp_tree_tib <- add_tree_traits(ausdata_all_pos_sp_tree_tib,
                                               avg_all_pos_sp_data)

rm(ausdata_all_pos_sp_df, all_pos_sp_data)
# end of all pos sp data entry

# fab tree only entry

fab_tree <- read.tree(here("Inputs/Trees/fab.tre"))
fab_data <- aus_data %>% subset(family == "Fabaceae")
#variation columns already included
avg_fabdata <- average_nutrient_data(select_relevant_columns(aus_data))

fab_tree_tib <- as_tibble(fab_tree)
fab_tree_tib <- add_tree_traits(fab_tree_tib, avg_fabdata)

write.csv(fab_tree_tib, file = "fab_tree_tib.csv")

# pruned tree data entry ----
auspruned_three_tree <- read.tree(here("Inputs/Trees/austraits_pruned_three.tre"))

pruned_ausdata_three <- prune_ausdata(aus_data, 3)

pruned_three_data <- add_CV_columns(select_relevant_columns(pruned_ausdata_three))

avg_pruned_three_data <- average_nutrient_data(pruned_three_data)

pruned_three_tree_tib <- as_tibble(auspruned_three_tree)
pruned_three_tree_tib <- add_tree_trait(pruned_three_tree_tib,
                                        avg_pruned_three_data)
# end of pruned tree data entry


# no gymn tree data entry ----
nogymn_tree <-read.tree(here("Inputs/Trees/no_gymnosperm_tree.tre"))
ausdata_no_gymn #from 001 Data Exploration
ausdata_no_gymn <- add_CV_columns(select_relevant_columns(ausdata_no_gymn))
avg_no_gymn <- average_nutrient_data(ausdata_no_gymn)

nogymn_tree_tib <- as_tibble(nogymn_tree)
nogymn_tree_tib <- add_tree_traits(nogymn_tree_tib, avg_no_gymn)
# end of no gymn data entry


# ITS tree data entry ----
ITS_tree <- read.nexus("Inputs/Trees/ITS_tree.tre")
ITS_tree_tib <- as_tibble(ITS_tree)

ITS_sp_data <- aus_data[aus_data$species_binom %in%
                          ITS_tree_tib$label, ]

ITS_sp_data <- select_relevant_columns((ITS_sp_data))

ITS_sp_data <- add_CV_columns(ITS_sp_data)
avg_ITS_sp_data <- average_nutrient_data(ITS_sp_data)

ITS_tree_tib <- add_tree_traits(ITS_tree_tib, avg_ITS_sp_data)
# end of ITS tree data entry


#----------
?phylANOVA

sp <- ausdata_tree$tip.label
#get named numeric vectors
x <- setNames(aus_data$myc_type[match(sp, aus_data$species_binom)], sp)
y <- setNames(aus_data$leaf_N_per_dry_mass[match(sp, aus_data$species_binom)], sp)

# run phylANOVA
phylANOVA(ausdata_tree, x, y)

phylANOVA(ausdata_tree, x = aus_data$myc_type,
          y = aus_data$leaf_N_per_dry_mass)

anova <- aov(leaf_N_per_dry_mass ~ myc_type, data = aus_data)
summary(anova)
#pre-lim thoughts: check that everything is set up correctly (trait order)
#but seems like ANOVA significant in detecting difs b/w myc_type
#but phylANOVA doesnt ... so these differences once evolutionary history taken into account
#dont exist across myc types


#-------
?phyl.pca
#can only take one value per species
avg_ausdata <- as.data.frame(avg_ausdata)
row.names(avg_ausdata) <- avg_ausdata$species_binom
ausdata_tree

# 2. Drop the species column, keep only nutrients
nutrient_matrix <- avg_ausdata[, c("avg_leaf_N", "avg_leaf_P", "avg_leaf_C")]
nutrient_matrix <- na.omit(nutrient_matrix)

# 3. Scale (center + standardize) each nutrient column
Y <- scale(nutrient_matrix)


pca <- phyl.pca(tree = ausdata_tree, Y, method = "lambda", mode = "cov")
pca


#try and plot by myc_type
scores <- as.data.frame(pca$S)   # species x PCs
scores$species <- rownames(scores)

plot_data <- merge(scores,
                   avg_ausdata[, c("species_binom", "myc_type")],
                   by.x = "species", by.y = "species_binom")

ggplot(plot_data, aes(x = PC1, y = PC2, color = myc_type)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = myc_type), linetype = 2) +  # circles around groups
  theme_minimal() +
  labs(x = "PC1", y = "PC2",
       title = "Phylogenetic PCA of Leaf Nutrients") +
  theme(legend.position = "right")

plot_data <- na.omit(plot_data)
adonis_res <- adonis2(plot_data[, c("PC1", "PC2", "PC3")] ~ myc_type,
                      data = plot_data, method = "euclidean")
adonis_res

disp <- betadisper(
  dist(plot_data[, c("PC1", "PC2", "PC3")]),
  group = plot_data$myc_type
)
anova(disp)

TukeyHSD(disp)
plot(disp) 

#unconstrained RDA = PCA
pca_raw <- vegan::rda(Y)
pca_raw

library(vegan)
library(ggplot2)

# 1. Extract site scores (species)
scores_sites <- as.data.frame(vegan::scores(pca_raw, display = "sites"))
scores_sites$species <- rownames(scores_sites)

# 2. Extract variable scores (nutrient loadings)
scores_vars <- as.data.frame(vegan::scores(pca_raw, display = "species"))
scores_vars$nutrient <- rownames(scores_vars)

# 3. Merge site scores with metadata
plot_data <- merge(scores_sites,
                   avg_ausdata[, c("species_binom", "myc_type")],
                   by.x = "species", by.y = "species_binom")

# 4. Plot PCA
ggplot(plot_data, aes(x = PC1, y = PC2, color = myc_type)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = myc_type), linetype = 2) +
  geom_segment(data = scores_vars,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               inherit.aes = FALSE, color = "black") +
  geom_text(data = scores_vars,
            aes(x = PC1, y = PC2, label = nutrient),
            inherit.aes = FALSE, vjust = -0.5) +
  theme_minimal() +
  labs(title = "PCA of Leaf Nutrients (vegan::rda)",
       x = "PC1", y = "PC2")
#uhhh kinda looks the same ?