library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(httpgd)
library(languageserver)
library(lintr)
library(hypervolume)
library(rgl)

#----hypervolumes
#ex. Soil N P and C
#ex. Leaf N, P, and C
# major families different in these quantities

env1 <- "SN_total_0_30"
env2 <- "SP_total_0_30"
env3 <- "SOC_total_0_30"
leaf_trait <- "leaf_N_per_dry_mass"

#subset data to only 3 env predictors + leaf trait
subsetvol <- aus_data %>%
  select(all_of(c(env1, env2, env3, leaf_trait)))

#how leaf trait varies with env conditions
hv_entire <- (hypervolume(scale(subsetvol), name = "Entire Dataset"))

#Environment space occupied by all observations only
hv_env <- hypervolume(scale(subsetvol%>%select(all_of(c(env1, env2, env3)))), name = "Env")
#this takes a long time - 5 min?

# Plot the hypervolume for the entire dataset
plot(hv_entire, show.3d = FALSE)
plot(hv_entire, show.3d = TRUE) # yay it works!
plot(hv_env, show.3d = T)


#mess around with env
#remember no NAs, only numerical inputs
#to have seperate colors per family, need one cube object unique to that family
#remember to scale cube! 
#then merge cube objects using hypervolume_join()
#then plot that merged object


#Myrtaceae, Fabaceae, Proteaceae, prep for hypervol
myr <- scale(na.omit(subset((aus_data), family == "Myrtaceae") %>%
                       select(all_of(c(env1, env2, env3, leaf_trait)))))
myr_cube <- (hypervolume((myr), name = "Myrtaceae"))

fab <- scale(na.omit(subset((aus_data), family == "Fabaceae") %>%
                       select(all_of(c(env1, env2, env3, leaf_trait)))))
fab_cube <- (hypervolume((fab), name = "Fabaceae"))

pro <- scale(na.omit(subset((aus_data), family == "Proteaceae") %>%
                       select(all_of(c(env1, env2, env3, leaf_trait)))))
pro_cube <- (hypervolume((pro), name = "Proteaceae"))

fam_cube <- hypervolume_join(myr_cube, fab_cube, pro_cube)
plot(fam_cube, show.3d = T, colors = c("gold", "red", "blue"), point.alpha.min = 0.9)
plot(fam_cube, show.3d = F, colors = c("green", "red", "blue"))


#try cubes of most common sp
#corymbia_calophylla, eucalyptus_tereticornis, eucalyptus_tetrodonta, corymbia_terminalis
#eucalyptus_miniata, eucalyptus_macrorhyncha, acacia_aneura
cor <- scale(na.omit(subset((aus_data), species_binom == "corymbia_calophylla") %>%
                       select(all_of(c(env1, env2, env3, leaf_trait)))))
cor_cube <- (hypervolume((cor), name = "Corymbia Calophylla"))


#try most common genera?