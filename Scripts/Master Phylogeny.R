install.packages("devtools")
library(devtools)
devtools::install_github("jinyizju/V.PhyloMaker")
library(ape)
library(V.PhyloMaker)
library(dplyr)

setwd("/Users/sofiaquijada/Documents/McGill/2024 Soper Lab/AusStoich")

#goal: make a genus-level phylogeny for all plants in Austraits

#using separate csv file since V.Phylomaker has _ in their species names
austraits_leaf_stoich <-
  read.csv("Inputs/austraits_leaf_stoichiometry_MASTER_for_phylogeny.csv")


# ------------------------- V.PhyloMaker ------------------------- #
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
# ------------------------- V.PhyloMaker 2 ------------------------- #
devtools::install_github("jinyizju/V.PhyloMaker2")
library(V.PhyloMaker2)
#package has three tre files: how many species in austraits are in these databases?
#species level, and genus level

#-------- TPL database --------#
sp_in_aus_not_in_tpl_2 <- austraits_leaf_stoich$species_binom[!(austraits_leaf_stoich$species_binom
                                        %in% tips.info.TPL$species)]#3072
sp_in_aus_not_in_tpl_2 <- sort(unique(sp_in_aus_not_in_tpl_2))
length(sp_in_aus_not_in_tpl_2) #619

#genus level
TPL_genus_2 <- tips.info %>%
  mutate(
    family_genus = paste(tips.info$family,
                         tips.info$genus,sep=" "))

sp_in_aus_not_in_tpl_genus_2 <- 
  austraits_leaf_stoich_genus$family_genus[!(austraits_leaf_stoich_genus$family_genus
                                             %in% TPL_genus_2$family_genus)]
sp_in_aus_not_in_tpl_genus_2 <- sort(unique(sp_in_aus_not_in_tpl_genus_2)) 
length(sp_in_aus_not_in_tpl_genus_2)#58, same as V.PhyloMaker1


#-------- LCVP database --------#
# this database has a package available for name standardization
devtools::install_github("idiv-biodiversity/LCVP")
devtools::install_github("idiv-biodiversity/lcvplants")
library(lcvplants)

sp_in_aus_not_in_LCVP <- austraits_leaf_stoich$species_binom[!(austraits_leaf_stoich$species_binom
                                           %in% tips.info.LCVP$species)]#3158
sp_in_aus_not_in_LCVP <- sort(unique(sp_in_aus_not_in_LCVP))
length(sp_in_aus_not_in_LCVP) #613
sp_in_aus_not_in_LCVP <- data.frame(sp_in_aus_not_in_LCVP)

#genus level
LCVP_genus <- tips.info %>%
  mutate(
    family_genus = paste(tips.info$family,
                         tips.info$genus,sep=" "))

sp_in_aus_not_in_LCVP_genus <- 
  austraits_leaf_stoich_genus$family_genus[!(austraits_leaf_stoich_genus$family_genus
                                             %in% TPL_genus_2$family_genus)]
sp_in_aus_not_in_LCVP_genus <- sort(unique(sp_in_aus_not_in_LCVP_genus))
length(sp_in_aus_not_in_LCVP_genus) #58
sp_in_aus_not_in_LCVP_genus <- data.frame(sp_in_aus_not_in_LCVP_genus)

#-------- WP database --------#
sp_in_aus_not_in_WP <- austraits_leaf_stoich$species_binom[!(austraits_leaf_stoich$species_binom
                                                %in% tips.info.WP$species)] #3102
sp_in_aus_not_in_WP <- unique(sp_in_aus_not_in_WP) 
length(sp_in_aus_not_in_WP)#619

WP_genus <- tips.info %>%
  mutate(
    family_genus = paste(tips.info$family,
                         tips.info$genus,sep=" "))
sp_in_aus_not_in_WP_genus <- 
  austraits_leaf_stoich_genus$family_genus[!(austraits_leaf_stoich_genus$family_genus
                                             %in% TPL_genus_2$family_genus)]
sp_in_aus_not_in_WP_genus <- unique(sp_in_aus_not_in_WP_genus) 
length(sp_in_aus_not_in_WP_genus) #58
