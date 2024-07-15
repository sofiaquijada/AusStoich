install.packages("devtools")
library(devtools)
devtools::install_github("jinyizju/V.PhyloMaker")
library(ape)
library(V.PhyloMaker2)
library(dplyr)

setwd("/Users/sofiaquijada/Documents/McGill/2024 Soper Lab/AusStoich")

#goal: make a genus-level phylogeny for all plants in Austraits

#using separate csv file since V.Phylomaker has _ in their species names
austraits_leaf_stoich <-
  read.csv("Inputs/austraits_leaf_stoichiometry_MASTER_for_phylogeny.csv")


##########---------V.PhyloMaker---------##########
#species in austraits all in GBOTB?
all(austraits_leaf_stoich$species_binom %in% tips.info$species) #FALSE

species_in_austraits_not_in_TPL <- 
  austraits_leaf_stoich$species_binom[!(austraits_leaf_stoich$species_binom
                                      %in% tips.info$species)] #3072 
length(unique(species_in_austraits_not_in_TPL)) #619

#manual search of species in austraits not in tree
#show up on plant list... but not in tree for some reason
#tree is supposed to be based on TPL -> inactive since 2013
#even on old 2013 site some species can be found

#ex
#Salsola_australis -> actually salsola_kali in TPL -> in tips.info
#Hypolepis_rugosula -> in TPL -> not in tree?


#making df with family and genus for austraits data & TPL plant list
austraits_leaf_stoich_genus <- austraits_leaf_stoich %>%
  mutate(
    family_genus = paste(austraits_leaf_stoich$family,
                         austraits_leaf_stoich$genus,sep=" "))

TPL_genus <- tips.info %>%
  mutate(
    family_genus = paste(tips.info$family,
                         tips.info$genus,sep=" "))

#see how many are in austraits that aren't in TPL tre
sp_in_aus_not_in_tpl_genus <- 
  austraits_leaf_stoich_genus$family_genus[!(austraits_leaf_stoich_genus$family_genus
                                        %in% TPL_genus$family_genus)]
length(unique(sp_in_aus_not_in_tpl_genus)) #58 


#try updated package
##########---------V.PhyloMaker2---------##########
devtools::install_github("jinyizju/V.PhyloMaker2")
library(V.PhyloMaker2)
#package has three tre files: how many species in austraits are in these databases?
#species level, and genus level

##########---------TPL database---------##########
sp_in_aus_not_in_tpl_2 <- austraits_leaf_stoich$species_binom[!(austraits_leaf_stoich$species_binom
                                        %in% tips.info.TPL$species)]#3072
sp_in_aus_not_in_tpl_2 <- sort(unique(sp_in_aus_not_in_tpl_2))
length(sp_in_aus_not_in_tpl_2) #619

#genus level
TPL_genus_2 <- tips.info.TPL %>%
  mutate(
    family_genus = paste(tips.info.TPL$family,
                         tips.info.TPL$genus,sep=" "))

sp_in_aus_not_in_tpl_genus_2 <- 
  austraits_leaf_stoich_genus$family_genus[!(austraits_leaf_stoich_genus$family_genus
                                             %in% TPL_genus_2$family_genus)]
sp_in_aus_not_in_tpl_genus_2 <- sort(unique(sp_in_aus_not_in_tpl_genus_2)) 
length(sp_in_aus_not_in_tpl_genus_2)#58, same as V.PhyloMaker1

##########--------WP database---------##########

sp_in_aus_not_in_WP <- austraits_leaf_stoich$species_binom[!(austraits_leaf_stoich$species_binom
                                                             %in% tips.info.WP$species)] #3102
sp_in_aus_not_in_WP <- unique(sp_in_aus_not_in_WP) 
length(sp_in_aus_not_in_WP) #619

WP_genus <- tips.info.WP %>%
  mutate(
    family_genus = paste(tips.info.WP$family,
                         tips.info.WP$genus,sep=" "))
sp_in_aus_not_in_WP_genus <- 
  austraits_leaf_stoich_genus$family_genus[!(austraits_leaf_stoich_genus$family_genus
                                             %in% TPL_genus_2$family_genus)]
sp_in_aus_not_in_WP_genus <- unique(sp_in_aus_not_in_WP_genus) 
length(sp_in_aus_not_in_WP_genus) #58

##########---------LCVP database - name standardization---------##########
# this database has a package available for name standardization
devtools::install_github("idiv-biodiversity/LCVP")
library(LCVP)
devtools::install_github("idiv-biodiversity/lcvplants")
library(lcvplants)

sp_in_aus_not_in_LCVP <- austraits_leaf_stoich$species_binom[!(austraits_leaf_stoich$species_binom
                                                               %in% tips.info.LCVP$species)]#3158
sp_in_aus_not_in_LCVP <- sort(unique(sp_in_aus_not_in_LCVP))
length(sp_in_aus_not_in_LCVP) #613

lcvp_search <- lcvp_search(sp_in_aus_not_in_LCVP)
#didn't work because "Ichnocarpus" is not binom

sp_in_aus_not_in_LCVP_no_ich <- sp_in_aus_not_in_LCVP [! 
                                sp_in_aus_not_in_LCVP %in% c("Ichnocarpus")]
lcvp_search <- lcvp_search(sp_in_aus_not_in_LCVP_no_ich)
lcvp_summary(lcvp_search)

lcvp_fuzzy_srch <- lcvp_fuzzy_search(sp_in_aus_not_in_LCVP_no_ich, progress_bar = TRUE)
lcvp_summary(lcvp_fuzzy_srch) #for some reason no species included here

#genus level
LCVP_genus <- tips.info.LCVP %>% 
  mutate(
    family_genus = paste(tips.info.LCVP$family,
                         tips.info.LCVP$genus,sep=" "))

sp_in_aus_not_in_LCVP_genus <- 
  austraits_leaf_stoich_genus$family_genus[!(austraits_leaf_stoich_genus$family_genus
                                             %in% LCVP_genus$family_genus)] 
sp_in_aus_not_in_LCVP_genus <- sort(unique(sp_in_aus_not_in_LCVP_genus))
length(sp_in_aus_not_in_LCVP_genus) #59
#this has list of Family Genus whose family_genus entries are not in LCVP genus
#there could be many species with these missing/wrong family genus entries

sp_in_aus_in_LCVP_genus <- intersect(austraits_leaf_stoich_genus$family_genus, LCVP_genus$family_genus)
sp_in_aus_in_LCVP_genus <- sort(unique(sp_in_aus_in_LCVP_genus))
length(sp_in_aus_in_LCVP_genus) #372, which is 431 - 59
  
austraits_genus <- austraits_leaf_stoich_genus %>%
  select(species = species_binom, genus, family) #includes ichnocarpus entry

#remove double entries
austraits_genus <- austraits_genus %>%
  distinct(species, .keep_all = TRUE) #1421 species

#need to make a database of austraits_genus w/o lcvp_search species
#lcvp_search species = sp_in_aus_not_in_LCVP_no_ich

sp_in_aus_not_in_LCVP_df <- data.frame(sp_in_aus_not_in_LCVP) 
#includes ichnocarpus entry

#remove lcvp_search species
austraits_wo_lcvp_search <- austraits_genus %>%
  anti_join(sp_in_aus_not_in_LCVP_df, 
            by = c("species" = "sp_in_aus_not_in_LCVP")) #Ichnocarpus not included

write.csv(austraits_wo_lcvp_search, "Inputs/austraits_wo_lcvp_search.csv",
          row.names = FALSE)

#joining dataframes done in excel
#austraits_wo_lcvp_search added to corrected lcvp_search species &
#to manual correction of genus

#first run through of phylo.maker still had 11 family names to fix
#done manually in excel

##########---------LCVP naming summary---------##########

#want to know how many entries from leaf_stoich are lost using these final names
austraits_raw_genus <- austraits_leaf_stoich_genus %>% 
  select(species = species_binom, genus, family) #7826 entries
#need to correct 46 names according to all_naming_corrections.csv 

all_naming_corrections <- read.csv("Inputs/all_naming_corrections.csv")

austraits_corrected <- austraits_raw_genus %>%
  left_join(all_naming_corrections, by = c("species" = "species_before_correction",
                                           "genus" = "genus_before_correction",
                                           "family" = "family_before_correction")) %>%
  mutate(
    species = ifelse(!is.na(species_after_correction), species_after_correction, species),
    genus = ifelse(!is.na(genus_after_correction), genus_after_correction, genus),
    family = ifelse(!is.na(family_after_correction), family_after_correction, family)
  ) %>%
  select(species, genus, family)

sp_in_cor_aus_not_in_LCVP <- austraits_corrected$species[!(austraits_corrected$species
                                                           %in% tips.info.LCVP$species)]
length(sp_in_cor_aus_not_in_LCVP) #2997
length(unique(sp_in_cor_aus_not_in_LCVP)) #587, same number as failed lcvp_search names
#makes sense

sp_in_cor_aus_not_in_LCVP_no_ich <- sp_in_cor_aus_not_in_LCVP [! 
                                                                 sp_in_cor_aus_not_in_LCVP
                                                               %in% c("Ichnocarpus")]
lcvp_sanity_check <- lcvp_search(sp_in_cor_aus_not_in_LCVP_no_ich)
lcvp_summary(lcvp_sanity_check) #same ones that are missing, so all good 

austraits_unique_corrected <- austraits_corrected %>%
  distinct(species, .keep_all = TRUE) #1415 species, not 1421 (probably due to synonyms)

##########---------austraits_leaf_stoich Data Tidying---------##########

#name correction
austraits_leaf_stoich <- austraits_raw_genus %>%
  left_join(all_naming_corrections, by = c("species" = "species_before_correction",
                                           "genus" = "genus_before_correction",
                                           "family" = "family_before_correction")) %>%
  mutate(
    species = ifelse(!is.na(species_after_correction), species_after_correction, species),
    genus = ifelse(!is.na(genus_after_correction), genus_after_correction, genus),
    family = ifelse(!is.na(family_after_correction), family_after_correction, family)
  ) %>%
  select(species, genus, family)

#outliers (nutrient outliers)


#structure analysis



##########---------austraits_all_pos_sp.tre---------##########

austraits_all_pos_sp_df <- read.csv("Inputs/all_pos_austraits_LCVP_sp.csv")

austraits_all_pos_sp <- phylo.maker(sp.list = austraits_all_pos_sp_df,
                                           tree = GBOTB.extended.LCVP,
                                           nodes = nodes.info.1.LCVP,
                                           scenarios="S3")

write.tree(austraits_all_pos_sp$scenario.3,
           "Inputs/austraits_all_pos_sp.tre")

austraits_all_pos_sp_tree<- read.tree("Inputs/austraits_all_pos_sp.tre")
plot(austraits_all_pos_sp_tree, cex= 0.1) #this has all possible species in it

austraits_all_pos_sp_tree_tib <- as_tibble(austraits_all_pos_sp_tree)
#use left join to join with nutrient data 

##########---------austraits_one_rep_per_gen.tre & genera lost---------##########

#this tree will have one species representative per genus 
#431 total genera, so csv should have 431 - some number entries

#set seed for reproducibility
set.seed(111)
austraits_one_rep_per_gen_df <- austraits_all_pos_sp_df %>%
  group_by(genus) %>%
  slice_sample(n = 1) %>%
  ungroup()
austraits_one_rep_per_gen_df <- data.frame(austraits_one_rep_per_gen_df) #not tibble
#286 entries??? way too low... 

#431 - 286 = 145

gen_in_allposp_not_in_one_rep <- 
  austraits_all_pos_sp_df$genus[!(austraits_all_pos_sp_df$genus
                                                  %in% austraits_one_rep_per_gen$genus)]#0 
length(unique(austraits_genus$genus)) #431 

gen_in_aus_not_in_one_rep<- data.frame(austraits_unique_corrected$genus[!(austraits_unique_corrected$genus
                                        %in% austraits_one_rep_per_gen$genus)]) #219
#219 will include names not found to species level
#make tree anyway

austraits_one_rep_per_gen <- phylo.maker(sp.list = austraits_one_rep_per_gen_df,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios="S3")

write.tree(austraits_one_rep_per_gen$scenario.3,
           "Inputs/austraits_one_rep_per_gen.tre")

austraits_one_rep_per_gen_tree<- read.tree("Inputs/austraits_one_rep_per_gen.tre")
plot(austraits_one_rep_per_gen_tree, cex= 0.1)







