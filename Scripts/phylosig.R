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
library(arsenal) #comparedf() function useful
library(readr)

#needed to do this to install ggtree for some reason
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")


aus_data # from 02-Data-Import

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

all_pos_sp_data <- add_CV_columns(select_relevant_columns(all_pos_sp_data))

avg_all_pos_sp_data <- average_nutrient_data(all_pos_sp_data)

ausdata_all_pos_sp_tree_tib <- as_tibble(ausdata_all_pos_sp_tree)
ausdata_all_pos_sp_tree_tib <- add_tree_traits(ausdata_all_pos_sp_tree_tib,
                                                    avg_all_pos_sp_data)

rm(ausdata_all_pos_sp_df, all_pos_sp_data)
# end of all pos sp data entry


# ausdata data entry ----
ausdata_tree <- read.tree(here("Inputs/Trees/ausdata.tre"))
aus_data
ausdata_nut <- add_CV_columns(select_relevant_columns(aus_data))
avg_ausdata <- average_nutrient_data(ausdata_nut)

ausdata_tree_tib <- as_tibble(ausdata_tree)
ausdata_tree_tib <- add_tree_traits(ausdata_tree_tib, avg_ausdata)

rm(ausdata_nut, avg_ausdata)
# end of ausdata data entry


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
#cut = 1404
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


#-- Linkage of data to phylo object
#need info df with column "label" then trait data as columns
names(avg_all_pos_sp_data)[1] <- "label"
attemptree <- full_join(as.treedata(ausdata_all_pos_sp_tree),
                        avg_all_pos_sp_data, by = "label")
attemptree
View(as.tibble(attemptree))

#following: tree tib into tree data also works!
attemptree1 <- full_join(as.treedata(aus_all_pos_sp_tree_tib), avg_all_pos_sp_data, by = "label")
get.data(attemptree) #to extract data from phylo
  
ggtree(attemptree, aes(color = avg_leaf_N)) +
  scale_color_continuous(low = "yellow", high = "magenta") +
  geom_tiplab(size = 0.5, color = "black")

#only plots existing values
#use this since one color per branch
ggtree(attemptree, aes(color = avg_leaf_N), layout = "circular") +
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00") +
  geom_tiplab(size = 0.5) #can manually set to black if needed

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

lambda <- phylosig(tree, trait_data,method = "lambda", test = TRUE)
#phylosig(se = ), from Ellie's code
#can set se manually = to value per species! 
print(lambda)
plot(lambda)

loglambda_signal <- phylosig(tree, logged_trait_data,
                             method = "lambda", test = TRUE)
print(loglambda_signal)