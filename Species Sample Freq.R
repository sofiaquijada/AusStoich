austraits_leaf_stoichiometry_MASTER<-read.csv("austraits_leaf_stoichiometry_MASTER.csv")
austraits_leaf_stoichiometry_MASTER

summary(austraits_leaf_stoichiometry_MASTER)

install.packages("tidyverse")
library(tidyverse)



###### species and number of samples - by genus ######
# number of species on left, species as indep variable 

# first, combine family and genus into one row 
austraits_leaf_stoichiometry_MASTER$species <- paste(austraits_leaf_stoichiometry_MASTER$family,
                                                     austraits_leaf_stoichiometry_MASTER$genus, sep = " ") 
#csv file not changed, this is uniquely in this R script. now family and genus are combined into "species", 
#as an extra column at the end

#step one: make vector of species 
species<-austraits_leaf_stoichiometry_MASTER$species
print(species)
#all the repeats should be counted 
species_count <- table(species)
print(species_count)
typeof(species_count)
#10464 samples

species_count_df<-as.data.frame(species_count)
#get rid of first row: no species, freq 10464 

# species_count_df <- species_count_df[2:,] #every row after the second, all columns didnt work
species_count_df <- species_count_df[-c(1), ]
#first row removed
typeof(species_count_df)
print(species_count_df)

species_names <- species_count_df[c("species")] #no repeats 
typeof(species_names) #list
print(species_names)
species_samples <- species_count_df[c("Freq")]
typeof(species_samples) #list
print(species_samples)
typeof(species_count)


# 10464 samples
# 431 species by genus

ggplot(data = species_count_df) + 
  geom_point(mapping = aes(x = species, y = Freq)) + ggtitle("Number of Samples per Species, by genus")
#flag: so many more samples
# 	Myrtaceae Eucalyptus (1800) 
#   Myrtaceae Corymbia (754)
#   Fabaceae Acacia (676)

#make plot without outliers, see what the average is there
species_count_df_no_outliers <- species_count_df[-c(153,265,267), ]
ggplot(data = species_count_df_no_outliers) + 
  geom_point(mapping = aes(x = species, y = Freq))+ ggtitle("Number of Samples per Species, no outliers\n by genus")
#plot without 3 main outliers  

#average number of samples without outliers 
avg_sample_no_outliers <- mean(species_count_df_no_outliers$Freq)
print(avg_sample_no_outliers) #10.73832



###### species and number of samples - by species binom ######
species_binom <- austraits_leaf_stoichiometry_MASTER$species_binom
typeof(species_binom)
print(species_binom)

species_count_binom <- table(species_binom)
print(species_count_binom)
typeof(species_count_binom)

species_count_df_binom<-as.data.frame(species_count_binom)
#remove first row, 
species_count_df_binom<- species_count_df_binom[-c(1), ]
#10464 samples
#1421 species by binom

ggplot(data = species_count_df_binom) + 
  geom_point(mapping = aes(x = species_binom, y = Freq)) + ggtitle("Number of Samples per Species, binom")
#outlier:Corymbia calophylla (477) which is Myrtaceae Corymbia

#without outlier
species_count_df_binom_no_outliers <- species_count_df_binom[-c(318), ]
ggplot(data = species_count_df_binom_no_outliers) + 
  geom_point(mapping = aes(x = species_binom, y = Freq))+ ggtitle("Number of Samples per Species, no outliers\n binom")
#average number of samples without outliers 
avg_sample_no_outliers_bn <- mean(species_count_df_binom_no_outliers$Freq)
print(avg_sample_no_outliers_bn) #5.175352