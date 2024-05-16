install.packages("tidyverse")
library(tidyverse)

#tidy data rules:
# 1. Put each dataset in a tibble.
# 2. Put each variable in a column.
# 3. For each plot, subset relevant information. 



austraits_leaf_stoichiometry_MASTER<-read.csv("austraits_leaf_stoichiometry_MASTER.csv")

austraits_leaf_stoich_tib<-as_tibble(austraits_leaf_stoichiometry_MASTER)

################## Number of Samples per Species ###################

#alternate method to count sample number of species:
#count <- austraits_leaf_stoich_tib %>% count(austraits_leaf_stoich_tib$species_binom)

species_binom <- austraits_leaf_stoichiometry_MASTER$species_binom
#count sample number of species
species_count_tab <- table(species_binom)
#table to df
species_count_df<-as.data.frame(species_count_tab)
#df to tib
species_count_tib <- as_tibble(species_count_df)
#order from high to low
#species_count_tib <- species_count_tib%>%arrange(Freq)


ggplot(data=species_count_tib, aes(x = reorder(species_binom,-Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.5, fill="darkgreen") +
  ggtitle("Number of Samples per Species")+
  coord_flip()


#outlier:Corymbia calophylla which is Myrtaceae Corymbia

ggplot(data = subset(species_count_tib, Freq <= 200), aes(x = reorder(species_binom, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.5, fill="darkgreen") +
  ggtitle("Number of Samples per Species, no outlier") +
  coord_flip()
  
#summary data
#10464 samples
#1421 species
#5.175352 average without main outlier



#idea: faceting by family things like leaf N or 






