library(here)
library(tidyverse)
library(dplyr)
library(ape)
library(ggtree)
library(tidytree)
library(V.PhyloMaker2)


#---------ausdata_all_pos_sp.tre derivation---------

#edit: must validate before proper use

#all_pos_austraits_LCVP_sp manually made
#head: species	genus	family	species.relative	genus.relative

#need to derive this dataframe in code, from aus_data
#code copied from naming standardization validation

#species in ausdata not in tree
sp_mismatch <- unique(setdiff(aus_data$species_binom, tips.info.LCVP$species))

#species not in tree removed, placed in format for tree making
ausdata_all_pos_sp <- aus_data %>%
  select(species = species_binom, genus, family) %>%
  distinct(species, .keep_all = TRUE) %>%
  anti_join(as.data.frame(sp_mismatch), by = c("species" = "sp_mismatch")) %>%
  mutate(species.relative = NA, genus.relative = NA)
#length 829

ausdata_all_pos_sp <- phylo.maker(sp.list = ausdata_all_pos_sp,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios = "S3")

write.tree(ausdata_all_pos_sp$scenario.3, "Inputs/Trees/ausdata_all_pos_sp.tre")


#-- Old derivation
austraits_all_pos_sp_df <- read_csv(here("Inputs",
                                         "all_pos_austraits_LCVP_sp.csv"))
#manually derived

austraits_all_pos_sp <- phylo.maker(sp.list = austraits_all_pos_sp_df,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios = "S3")
#with this object can write .tre file, however it is already in Inputs
#write.tree(austraits_all_pos_sp$scenario.3, "Inputs/Trees/austraits_all_pos_sp.tre")
austraits_all_pos_sp_tree <- read.tree(here("Inputs/Trees/austraits_all_pos_sp.tre"))


#---------pruned_aus_data tree derivation---------

#species with less than or equal to three entries

pruned_ausdata_three <- prune_ausdata(aus_data, 3)

pruned_three_prepped <- prune_prep_tree(pruned_ausdata_three)

pruned_three_tree <- phylo.maker(sp.list = pruned_three_prepped,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios = "S3")

write.tree(pruned_three_tree$scenario.3, "Inputs/Trees/austraits_pruned_three.tre")

auspruned_three_tree <- read.tree(here("Inputs/Trees/austraits_pruned_three.tre"))


#---------no gymnosperm tree derivation---------

#from 01
ausdata_no_gymn<- aus_data%>% 
  filter(!family %in% c("Podocarpaceae", "Cupressaceae",
                        "Araucariaceae", "Zamiaceae", "Cycadaceae"))

nogymn_prep <- prune_prep_tree((ausdata_no_gymn))
nogymn_tree <- phylo.maker(sp.list = nogymn_prep,
                                               tree = GBOTB.extended.LCVP,
                                               nodes = nodes.info.1.LCVP,
                                                scenarios="S3")

write.tree(nogymn_tree$scenario.3, "Inputs/Trees/no_gymnosperm_tree.tre")

nogymn_tree <-read.tree(here("Inputs/Trees/no_gymnosperm_tree.tre"))

#--------aus_data only---------

#from 02
aus_data

ausdata_tree <- prune_prep_tree(aus_data)

ausdata_tree <- phylo.maker(sp.list = ausdata_tree,
                         tree = GBOTB.extended.LCVP,
                         nodes = nodes.info.1.LCVP,
                         scenarios="S3")
write.tree(ausdata_tree$scenario.3, "Inputs/Trees/ausdata.tre")
ausdata_tree <- read.tree(here("Inputs/Trees/ausdata.tre"))