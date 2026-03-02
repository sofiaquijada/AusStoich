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


# How to use: ------------------------------------------------------------------
#in the case we are using a new tree. Otherwise, conditionals already set up (end)

#1. Get aus_data-formatted object of interest
#       Only interested in nutrient columns: use select_relevant_columns()

#2. Write tree based on that object then read it into script
#       Prepare tree for writing using prune_prep_tree(), then write
#       This will be done in tree derivation script
#       Read tree as a tree tibble.

#3. Data entry - Add CV columns and get nutrient averages for aus_data object
#       Using add_CV_columns() then average_nutrient_data() on aus_data-obj
#       Note -  to prep all in one go, use:
#       select_relevant_columns(average_nutrient_data(add_CV_columns(aus_data))

#4. Merge trait data with tree tib object to compute signal
#       Using add_tree_traits()
#       Look at final object to determine row when trait data ends to determine
#       "cut" value for next step.

#5. Get trait values as named numerical vector, then compute signal
#      Use extract_trait_values() with "label" and "trait" unless otherwise specfied
#      as well as unique "cut" value previously determined
#      compute signal using phylosig()

#------------------------------Data Entry---------------------------------------

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


# ausdata data entry ----
ausdata_tree <- read.tree(here("Inputs/Trees/ausdata.tre"))
aus_data
#following line no longer needed, as relevant variation columns
#already in aus_data csv
#ausdata_nut <- add_var_columns(select_relevant_columns(aus_data))
avg_ausdata <- average_nutrient_data(select_relevant_columns(aus_data))

ausdata_tree_tib <- as_tibble(ausdata_tree)
ausdata_tree_tib <- add_tree_traits(ausdata_tree_tib, avg_ausdata)

rm(ausdata_nut, avg_ausdata)
# end of ausdata data entry

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


#-------------------------------------------------------------------------------

# Plots ----

# Trying to plot by genera (and failing) ---
#issue with plotting genera by color: tree has a bunch of random nodes not included
#in average species data information
#create tree object without this info

tree$node.label <- NULL

#for the following, need phylo object and dataframe associated with it

#horizontal base
ausdata_plot <- ggtree(ausdata_tree) + geom_tiplab(size = 0.5)

#most basic, no coloring, horizontal bar plot
ausdata_plot + geom_facet(
  panel = 'Trait',
  data = avg_all_pos_sp_data,
  geom = geom_col,
  mapping = aes(x = CV_C),
  orientation = "y") +
  ggtitle("CV_C, Tree with Uncertainties") +
  theme(plot.title = element_text(size = 20))

#try to color continously by trait
#syntax used: but we dont have trait data to match on phylo object... 
#must link using treeio, full_join() method


#-- Linkage of data to phylo object with treeio
#need info df with column "label" then trait data as columns
names(avg_all_pos_sp_data)[1] <- "label"
attemptree <- full_join(as.treedata(ausdata_all_pos_sp_tree),
                        avg_all_pos_sp_data, by = "label")
attemptree #tree object with nutrient data associated
View(as.tibble(attemptree))
get.data(attemptree) #to extract data from phylo
  
ggtree(attemptree, aes(color = avg_leaf_N)) +
  scale_color_continuous(low = "yellow", high = "magenta") +
  geom_tiplab(size = 0.5, color = "black")

#only plots existing values
#use this since one color per branch
ggtree(attemptree, aes(color = avg_leaf_N), layout = "circular") +
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00") +
  geom_tiplab(size = 0.5) #can manually set to black if needed

#trying sept 1st-----
treedata <- as.treedata(ausdata_tree_tib)

ggtree(treedata, aes(color = avg_leaf_N), layout = "circular") +
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00") +
  geom_tiplab(size = 0.5)

#by category!!
ggtree(treedata, aes(color = myc_type), layout = "circular") +
  geom_tiplab(size = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right") +
  labs(color = "Mycorrhizal type", shape = "Mycorrhizal type")

ggtree(treedata, aes(color = reclass_life_history), layout = "circular") +
  geom_tiplab(size = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right") +
  labs(color = "Life History", shape = "Life History")

ggtree(treedata, aes(color = factor(woodiness)), layout = "circular") +
  geom_tiplab(size = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right") +
  labs(color = "Woodiness", shape = "Woodiness")

ggtree(treedata, aes(color = factor(putative_BNF)), layout = "circular") +
  geom_tiplab(size = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right") +
  labs(color = "Nitrogen fixer", shape = "Nitrogen fixer")

#to label clades 
#https://yulab-smu.top/treedata-book/chapter5.html#layers-for-tree-annotation
#need to label internal nodes to use cladelab()
attemptree #829 tips, 828 internal nodes

#circular base
all_pos_sp_circular_plot <- ggtree(ausdata_all_pos_sp_tree, layout = "circular",
                                   branch.length = "none")+ ggtitle("All Pos. Sp.")

#most basic, no coloring circular bar plot
all_pos_sp_circular_plot + geom_fruit(
  data = avg_all_pos_sp_data,
  geom = geom_bar,
  mapping = aes(x = avg_leaf_N, y = species_binom),
  orientation = "y",
  stat = "identity") + ggtitle("Average Leaf N")


#------------------------Phylogenetic Signal------------------------------------

# 1. Pick tree, input as string. Options:

# "ITS_tree", cut = 105
# "ausdata_all_pos_sp", cut = 831, species-level
# "pruned_three", cut = 473
# "ausdata", cut = 1414, unresolved nodes
# Note that cut is inclusive i.e. up to and including

tree_tib <- "ausdata"

#write conditionals into function

if (tree_tib== "ausdata") {
  cut = 1414
  tree_tib = ausdata_tree_tib
  tree = ausdata_tree
}

if (tree_tib == "ausdata_all_pos_sp") {
  cut = 829
  tree_tib = ausdata_all_pos_sp_tree_tib
  tree = ausdata_all_pos_sp_tree
}

#derived from complete ausdata
if (tree_tib == "nogymn") {
  cut = 1403
  tree_tib = nogymn_tree_tib
  tree = nogymn_tree
}

if (tree_tib == "pruned_three") {
  cut = 473
  tree_tib = pruned_three_tree_tib
  tree = auspruned_three_tree
}

#earliest tree
if (tree_tib == "ITS_tree") {
  cut = 105
  tree_tib = ITS_tree_tib
  tree = ITS_tree
}


# 2. Write in trait of interest as string. Options:

# avg_leaf_N, avg_leaf_C or avg_leaf_P
# CV_N, CV_P, or CV_C
# avg_ar_NP_ratio, avg_ar_CN_ratio or avg_ar_CP_ratio
# avg_geo_NP_ratio, avg_geo_CN_ratio, avg_geo_CP_ratio

trait <- "avg_geo_NP_ratio"

# 3. Use extract_trait_values() on tree tib to get values of interest

trait_data <- extract_trait_values(tree_tib, "label", trait, cut)
logged_trait_data <- log(trait_data)

# 4. Get signals
K_signal <- phylosig(tree, trait_data, method = "K", nsim = 10000, test = TRUE)
print(K_signal)
quantile(K_signal$sim.K,c(0.05,0.95))
plot(K_signal)

logK_signal <- phylosig(tree, logged_trait_data, method = "K", nsim = 10000, test = TRUE)
print(logK_signal)
plot(logK_signal)

lambda <- phylosig(tree, trait_data, method = "lambda", test = TRUE)
#phylosig(se = ), from Ellie's code
#can set se manually = to value per species! 
print(lambda)
plot(lambda)

loglambda_signal <- phylosig(tree, logged_trait_data,
                             method = "lambda", test = TRUE)
print(loglambda_signal)


#---- investigation into differences between K and lambda
#One trick that could help us understand this apparent contradiction
#a bit better is to see what happens when we transform our second tree
#by the MLE of λ for that tree, and then compute K on the transformed tree

#"second tree" = tree with shortened terminal edges, slightly less than tip lengths
#tips of tree shortened by slightly less than 1/2 the distance b/w the two most 
#closely related sister taxa
lambda2<-phylosig(t2,x,method="lambda")$lambda
phylosig(phytools:::lambdaTree(t2,lambda2),x)

lambda2 <- phylosig(ausdata_tree, x = ausdata_tree_tib$avg_geo_NP_ratio,
                   method = "lambda")$lambdad
#get K for tree corrected by lambda MLE for that tree
phylosig(phytools:::lambdaTree(ausdata_tree,lambda2),ausdata_tree_tib$avg_geo_NP_ratio)
#suddently K is so much higher !!! what :3


#try ellie's method ----------
#just use one dataframe that has phylo (branch lengths), trait data, and SE 
ausdata_tree_tib <- ausdata_tree_tib %>%
  slice(1:1414)


lambda <- phylosig(ausdata_tree, x = ausdata_tree_tib$avg_geo_NP_ratio,
                   method = "lambda", test = TRUE)
#if you include se per species, given that not all of them have more than one observation
#get singular variance covariance matrix


#get seperate signals for distinct myc types: divide EM and AM
ausdata_tree_tib <- ausdata_tree_tib %>%
  slice(1:1414)


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