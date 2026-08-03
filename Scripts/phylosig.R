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
library(ggtreeExtra) #installed with BiocManager

httpgd::hgd() #VS code plot viewer
hgd_browse()

#rewrite so this script stands on its own. recompute phylosigs to make sure they
#are consistent for the complete data object
#and to have nice plots easily adaptable bc will need to do this for manu


#-------------------------------Functions---------------------------------------
select_relevant_columns <- function(df) {
  #function for removing environmental columns
  #in preparation for phylogenetic analysis
  df %>%
    select(
      -SN_total_0_30, -SP_total_0_30, -SOC_total_0_30,
      -CEC_total_0_30, -AP_total_0_30,
      -NPP, -MAT, -PPT, -AET,
      -precipitation_seasonality, -temp_seasonality
    )
}

geometric_mean <- function(x) {
  #function for calculating geometric mean
  #automatically excludes all NAs
  #Important note: use this function only for untransformed data
  #To get geometric mean of ln(ratio) data, use normal mean() function
  exp(mean(log(x), na.rm = TRUE))
}

average_nutrient_data <- function(df) {
  #function to average aus_data object nutrients and ratios
  #ratios averaged arithmetically and geometrically
  #note that only untransformed ratios should be averaged geometrically
  
  #avg = NaN means all entries for that species NA
  #species with one observation will have same value 
  nutrient_averaged_df <- df %>%
    
    group_by(species_binom) %>%
    
    summarize(
      avg_leaf_N = mean(leaf_N_per_dry_mass, na.rm = TRUE),
      avg_leaf_C = mean(leaf_C_per_dry_mass, na.rm = TRUE),
      avg_leaf_P = mean(leaf_P_per_dry_mass, na.rm = TRUE),
      avg_ar_NP_ratio = mean(NP_ratio, na.rm = TRUE),
      avg_ar_CN_ratio = mean(CN_ratio, na.rm = TRUE),
      avg_ar_CP_ratio = mean(CP_ratio, na.rm = TRUE),
      avg_geo_NP_ratio = geometric_mean(NP_ratio),
      avg_geo_CN_ratio = geometric_mean(CN_ratio),
      avg_geo_CP_ratio = geometric_mean(CP_ratio),
      
      #keep all columns, without this will get rid of the rest
      across(-c(leaf_N_per_dry_mass,leaf_C_per_dry_mass, leaf_P_per_dry_mass,
                NP_ratio, CN_ratio, CP_ratio),
             ~ first(.), .names = "{.col}")
    ) %>%
    ungroup() %>%
    #relocate all "avg_"columns to the right
    relocate(starts_with("avg_"), starts_with("geo_"), .after = last_col())
  
  #remove irrelevant unique ID column
  nutrient_averaged_df <- nutrient_averaged_df %>% select(-Unique_ID)
  
  return(nutrient_averaged_df)
}

add_tree_traits <- function(tree_tib, avg_sp_data) {
  #merging tree tib with trait data
  #written for getting phylo signal
  #avg_sp_data input = output of average_nutrient_data function
  merged_tib <- left_join(tree_tib, avg_sp_data, by = c("label" = "species_binom"))
  
  return(merged_tib)
}

extract_trait_values <- function(tree_tib, label_col, trait_col, cut) {
  # trait data must be in same order as label in tree
  # tree_tib: tree tibble object with associated trait data
  # label_col: name of the column that contains name of tip.labels from tree
  # trait_col: name of the column that has trait value of interest
  # cut: number of rows to keep from tree_tib
  
  # cut the tibble to the specified number of rows
  cut_tree_tib <- tree_tib %>%
    slice(1:cut)  #to ensure vector only includes nutrient values, not internal node info
  
  labels <- cut_tree_tib[[label_col]]
  traits <- cut_tree_tib[[trait_col]]
  
  trait_values <- setNames(as.numeric(traits), labels)
  #returns named numeric vector, of column of interest in the order 
  #of input of tree_tib
  return(trait_values)
}

#------------------------------Data Entry---------------------------------------

aus_data <- read_csv(file = "Inputs/aus_data2026.csv") 

#remove extra column
aus_data <- aus_data[,-1]

ausdata_tree <- read.tree(here("Inputs/Trees/ausdata.tre"))

#add column of sample size per species to aus_data
aus_data <- aus_data %>%
  group_by(species_binom) %>%
  mutate(sp_n = n()) %>%
  ungroup()

#average leaf nutrients, remove environmental columns
avg_ausdata <- average_nutrient_data(select_relevant_columns(aus_data))

#create tree tibble, merge with trait data
ausdata_tree_tib <- as_tibble(ausdata_tree)
ausdata_tree_tib <- add_tree_traits(ausdata_tree_tib, avg_ausdata)

#-------------------------------------------------------------------------------

# Plots ----

# Trying to plot by genera (and failing) ---
#issue with plotting genera by color: tree has a bunch of random nodes not included
#in average species data information
#create tree object without this info

tree$node.label <- NULL

#for the following, need phylo object and dataframe associated with it

#basic horizontal base ---- 
base_plot <- ggtree(ausdata_tree) + geom_tiplab(size = 0.5)

#plotting continuous coloring with ggtree
treedata <- as.treedata(ausdata_tree_tib)

ggtree(treedata, aes(color = avg_leaf_N), layout = "circular") +
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00") +
  geom_tiplab(size = 0.5)

#plotting by category with ggtree
ggtree(treedata, aes(color = myc_type), layout = "circular") +
  geom_tiplab(size = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "right") +
  labs(color = "Mycorrhizal type", shape = "Mycorrhizal type")

#circular base
circular_plot <- ggtree(ausdata_tree, layout = "circular", 
                        branch.length = "branch.length")

#most basic, no coloring circular bar plot
circular_plot + geom_fruit(
  data = avg_ausdata,
  geom = geom_bar,
  mapping = aes(x = avg_leaf_N, y = species_binom),
  orientation = "y",
  stat = "identity") + ggtitle("Average Leaf N")

#want to plot sample size on tips of trees
ggtree(treedata, layout = "circular", color = "grey30",size = 0.1) +
  geom_fruit(geom = geom_col,
    mapping = aes(y = label, x = sp_n)) #ugh finally

#consider this list:
species_observations <- species %>%
  group_by(Freq) %>%
  summarize(
    species_count = n(),
    species_list = list(toString(species_binom))
  ) %>%
  ungroup()

#red for freq 1, orange for freq 2, yellow for freq3, 
#4-6 green, #7-10 blue
#10-20, then 20-30, then 30-477 
treedata@data <- treedata@data %>%
  mutate(
    sample_bin = case_when(
      sp_n == 1 ~ "1",
      sp_n == 2 ~ "2",
      sp_n == 3 ~ "3",
      sp_n >= 4  & sp_n <= 6  ~ "4–6",
      sp_n >= 7  & sp_n <= 10 ~ "7–10",
      sp_n >= 11 & sp_n <= 20 ~ "11–20",
      sp_n >= 21 & sp_n <= 30 ~ "21–30",
      sp_n > 30 ~ "31+"
    ),
    sample_bin = factor(
      sample_bin,
      levels = c("1", "2", "3", "4–6", "7–10", "11–20", "21–30", "31+")
    )
  )

#sample size as circles, not ideal
ggtree(treedata, layout = "circular", color = "grey30", size = 0.1) +
  geom_tippoint(aes(fill = sample_bin), shape = 21,
                size = 1.2,color = "black", stroke = 0.1) +
  scale_fill_manual(name = "Sample size",
    values = c("1" = "red","2" = "orange","3" = "yellow","4–6" = "green",
      "7–10" = "blue", "11–20" = "purple", "21–30" = "brown", "31+" = "black"))

ggtree(treedata, layout = "circular",
       color = "grey40", linewidth = 0.1) + #tree base
  geom_fruit(geom = geom_col, mapping = aes(y = label, x = sample_bin, fill = sample_bin),
  ) +
  scale_fill_manual(
    name = "Frequency of Observations",
    values = c("1"     = "#d73027","2" = "#fc8d59","3"= "#fee08b",
      "4–6"   = "#91cf60","7–10"  = "#1a9850","11–20" = "#4575b4",
      "21–30" = "#542788", "31+"   = "#000000")) #these are bins, not absolute values...

#these are absolute values but... the corymbia obviously sets everything apart
ggtree(treedata, layout = "circular",
       color = "grey40", linewidth = 0.1) + #tree base
  geom_fruit(
    geom = geom_col,
    mapping = aes(y = label,
                  x = sp_n,
                  fill = sp_n))


#------------------------Phylogenetic Signal------------------------------------

# 1. Pick tree, input as string. Options:

# "ausdata", cut = 1414, unresolved nodes
# Note that cut is inclusive i.e. up to and including

tree_tib <- "ausdata"
cut = 1414
tree_tib = ausdata_tree_tib
tree = ausdata_tree

# 2. Write in trait of interest as string. Options:

# avg_leaf_N, avg_leaf_C or avg_leaf_P
# CV_N, CV_P, or CV_C
# avg_ar_NP_ratio, avg_ar_CN_ratio or avg_ar_CP_ratio
# avg_geo_NP_ratio, avg_geo_CN_ratio, avg_geo_CP_ratio

trait <- "ln_leaf_N"

# 3. Use extract_trait_values() on tree tib to get values of interest

trait_data <- extract_trait_values(tree_tib, "label", trait, cut)
logged_trait_data <- log(trait_data)

# 4. Get signals

#---lambda
lambda <- phylosig(tree, trait_data, method = "lambda", test = TRUE)
print(lambda)
plot(lambda)

loglambda_signal <- phylosig(tree, logged_trait_data,
                             method = "lambda", test = TRUE)
print(loglambda_signal)

#---bloomberg's K
K_signal <- phylosig(tree, trait_data, method = "K", nsim = 10000, test = TRUE)
print(K_signal)
quantile(K_signal$sim.K,c(0.05,0.95))
plot(K_signal)

logK_signal <- phylosig(tree, logged_trait_data, method = "K", nsim = 10000, test = TRUE)
print(logK_signal)
plot(logK_signal)

#alternatively:
ausdata_tree_tib <- ausdata_tree_tib %>%
  slice(1:1414)

lambda2 <- phylosig(ausdata_tree, x = ausdata_tree_tib$ln_leaf_N,
                   method = "lambda", test = TRUE)
#gives the same value, but will use the named numeric vector version
#just in case


#-------------------------------------------------------------------------------
#https://blog.phytools.org/2022/06/follow-up-on-sensitivity-of-blombergs-k.html 
#also consider:https://www.carlboettiger.info/2013/10/11/is-it-time-to-retire-pagels-lambda.html

#investigation into differences between K and lambda

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