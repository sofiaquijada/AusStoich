library(here)
library(tidyverse)
library(dplyr)
library(ape)
library(ggtree)
library(tidytree)
library(treeio)
library(ggtreeExtra)
library(phytools)
library(V.PhyloMaker2)

########------------------Data Import------------------########
# Ran 02 - Data import file
aus_data <- aus_data

########------------------Function Definitions------------------#######
#for parsing through aus_data
#input: dataframe in aus_data format, with only species of interest selected

#function for selecting relevant categorical and nutrient columns from aus_data
select_relevant_columns <- function(df) {
  selected_columns_df <- df[,c("species_binom", "family", "genus",
                               "woodiness", "reclass_life_history",
                               "putative_BNF", "myc_type",
                               "leaf_N_per_dry_mass", "leaf_P_per_dry_mass", 
                               "leaf_C_per_dry_mass")]
  return(selected_columns_df)
}

#function for adding coefficient of variation column
add_CV_columns <- function(df) {
  CV_added_df <- df %>%
    group_by(species_binom) %>%
    mutate(CV_N = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                               na.rm = TRUE),
           CV_P = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                               na.rm = TRUE),
           CV_C = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                               na.rm = TRUE))
  return(CV_added_df) 
}

#function for averaging nutrient data after adding covariance
average_nutrient_data <- function(df) {
  nutrient_averaged_df <- df %>%
    group_by(species_binom, family, genus, woodiness, reclass_life_history,
             putative_BNF, myc_type, CV_N, CV_P, CV_C) %>%
    summarize(
      avg_leaf_N = mean(leaf_N_per_dry_mass),
      avg_leaf_C = mean(leaf_C_per_dry_mass),
      avg_leaf_P = mean(leaf_P_per_dry_mass),
    )
  return(nutrient_averaged_df)
}


#########-------------austraits_all_pos_sp.tre derivation-------------#########
austraits_all_pos_sp_df <- read_csv(here('Inputs', 'Supplemental Inputs - Sofia',
                                         'all_pos_austraits_LCVP_sp.csv'))
#sesuvium_portulacastrum from all pos sp csv due to being an outlier

austraits_all_pos_sp <- phylo.maker(sp.list = austraits_all_pos_sp_df,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios="S3")
#with this object can write .tre file, however it is already in Inputs
#write.tree(austraits_all_pos_sp$scenario.3,
           #"Inputs/Trees/austraits_all_pos_sp.tre")

austraits_all_pos_sp_tree<- read.tree(here("Inputs/Trees/austraits_all_pos_sp.tre"))

########------------------Function Definitions------------------###
#for parsing through tree tib objects 
#extracting trait values for phylogenetic signals

#add ONLY family and genus columns function 
add_family_genus <- function(tree_tib, avg_sp_data) {
  merged_tib <- left_join(tree_tib, avg_sp_data, by = c("label" = "species_binom"))
  
  selected_tib <- merged_tib %>% 
    select(parent, node, branch.length, label, family, genus)
  
  return(selected_tib)
} #has to be used with AVERAGE traits - one entry per species only

add_relevant_colummns <- function(tree_tib, avg_sp_data) {
  merged_tib <- left_join(tree_tib, avg_sp_data, by = c("label" = "species_binom"))
  
  selected_tib <- merged_tib %>% 
    select(parent, node, branch.length, label, family, genus, woodiness,
           reclass_life_history, putative_BNF, myc_type, CV_N, CV_P, CV_C,
           avg_leaf_N, avg_leaf_C, avg_leaf_P)
  return(selected_tib)
}

extract_trait_values <- function(tree_tib, label_col, trait_col, cut) {
  # tree_tib: tree tibble object with associated trait data
  # label_col: name of the column that contains name of tip.labels from tree
  # trait_col: name of the column that has trait value of interest
  # cut: number of rows to keep from tree_tib
  
  # Cut the tibble to the specified number of rows
  cut_tree_tib <- tree_tib %>%
    slice(1:cut) #to ensure vector only includes nutrient values, not extra node info
  
  labels <- cut_tree_tib[[label_col]]
  traits <- cut_tree_tib[[trait_col]]
  
  trait_values <- setNames(as.numeric(traits), labels)
  #return named numeric vector, of column of interest in the order 
  #of input of tree_tib
  
  return(trait_values)
}

########---------austraits_all_pos_sp.tre plots---------########

#-----start of all_pos_sp_all_data derivation
all_pos_sp_data <- aus_data[aus_data$species_binom %in%
                                            austraits_all_pos_sp_df$species, ]

all_pos_sp_data <- select_relevant_columns(all_pos_sp_data)
all_pos_sp_data <- add_CV_columns(all_pos_sp_data)

#CV = NA can mean only one entry per that species
#CV = 0 means no variation for that species
#------end of all_pos_sp_all derivation


#------avg_all_pos_sp derivation
avg_all_pos_sp_data <- average_nutrient_data(all_pos_sp_data)
length(unique(avg_all_pos_sp_data$species_binom))
#-----end of avg_all_pos_sp derivation


all_pos_sp_plot <- ggtree(austraits_all_pos_sp_tree) +
  geom_tippoint(data = avg_all_pos_sp_data, mapping = aes(colour = family))
#node not found

#adding relevant columns onto tibble
aus_all_pos_sp_tree_tib <- as_tibble(austraits_all_pos_sp_tree)

aus_all_pos_sp_tree_tib <- add_family_genus(aus_all_pos_sp_tree_tib, avg_all_pos_sp_data)
#turn back into phylo class for plotting with tippoint

all_pos_sp_data_tree <- as.phylo(aus_all_pos_sp_tree_tib)
#bug
#https://github.com/YuLab-SMU/treeio/issues/36 

#bug solved manually here, using issues there
class(aus_all_pos_sp_tree_tib) <- c("tbl_tree", class(aus_all_pos_sp_tree_tib))
all_pos_sp_data_tree <- aus_all_pos_sp_tree_tib %>% as.phylo

##########---------Plotting---------##########
#horizontal base
all_pos_sp_plot <- ggtree(austraits_all_pos_sp_tree) + geom_tiplab(size = 0.5)

#most basic, no coloring, horizontal bar plot
all_pos_sp_plot + geom_facet(
  panel = 'Trait',
  data = avg_all_pos_sp_data,
  geom = geom_col,
  mapping = aes(x = avg_leaf_C),
  orientation = "y")  + ggtitle("Average Leaf N")

#ggtree(all_pos_sp_data_tree) #R ENCOUNTERS FATAL ERROR
#possibly because too many columns? fix function so that it adds 
#ONLY family and genus, and not the entire dataset

#ggtree(all_pos_sp_data_tree) #still encounters fatal error.... 
#cant plot trees this way. maybe another package can but not ggtree :c

ggtree(all_pos_sp_data_tree) +
  geom_tiplab(aes(label = label), size = 0.5) +  # Add tip labels
  geom_tippoint(aes(color = avg_leaf_N), size = 2) +  # Add tip points colored by trait value
  scale_color_gradient(low = "blue", high = "red") +  # Gradient color scale
  ggtitle("attempt") #fatal error

#circular base
all_pos_sp_circular_plot <- ggtree(austraits_all_pos_sp_tree, layout = "circular",
     branch.length = "none") + ggtitle("All Pos. Sp.")

#most basic, no coloring circular bar plot                                
all_pos_sp_circular_plot + geom_fruit(
  data = avg_all_pos_sp_data,
  geom = geom_bar,
  mapping = aes(x = avg_leaf_N, y = species_binom),
  orientation = "y",
  stat = "identity") + ggtitle("Average Leaf N") 



#scrap 
p <- ggtree(austraits_all_pos_sp_tree) + geom_tiplab() + xlim_tree(0.1)
plot(p)

ggtree(austraits_all_pos_sp_tree,layout='circular')
ggtree(austraits_all_pos_sp_tree, branch.length = "none",
       layout = "circular") + geom_tiplab(size = 0.7) + ggtitle("All Possible Species in tips.info.LCVP")
ggtree(austraits_all_pos_sp_tree, branch.length = "none",
       layout = "circular") + geom_nodelab()


##########---------austraits_one_rep_per_gen.tre & genera lost---------##########

austraits_one_rep_per_gen_tree<- read.tree(here("Inputs/Trees/austraits_one_rep_per_gen.tre"))
#derivation of this .tre in supplemental scripts - LCVP & early phylogeny
#ensure ichnocarpus is in here

plot(austraits_one_rep_per_gen_tree, cex= 0.1)
ggtree(austraits_one_rep_per_gen_tree, branch.length = "none",
       layout = "circular") + geom_tiplab(size = 2) + ggtitle("One Rep Per Genera in tips.info.LCVP")


##########---------Phylogenetic Signal---------##########

#for both phylosig and picante need fully resolved species tree to calculate
#trait data must be in same order as label in tree


#get tree tib seperate from one used to try to plot phylogenetic tree
#sig = for signal
aus_all_pos_sp_tree_tib_sig <- as_tibble(austraits_all_pos_sp_tree)
aus_all_pos_sp_tree_tib_sig <- add_relevant_colummns(aus_all_pos_sp_tree_tib_sig,
                                                     avg_all_pos_sp_data)


#extract_trait_values function on tree tib to get values of interest
trait_data <- extract_trait_values(aus_all_pos_sp_tree_tib_sig, "label", 
                                   "avg_leaf_N", 831)

K_signal <- phylosig(austraits_all_pos_sp_tree, trait_data,
                     method = "K", nsim = 10000) 
print(K_signal)
#note that number doesn't change depending on nsim
lambda <- phylosig(austraits_all_pos_sp_tree, trait_data,
                   method = "lambda") 
print(lambda)

#try it with 80 ITS tree


