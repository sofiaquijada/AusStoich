install.packages("tidyverse")
library(tidyverse)

#tidy data rules:
# 1. Put each dataset in a tibble.
# 2. Put each variable in a column.
# 3. For each plot, subset relevant information. 



raw_data<-read.csv("austraits_leaf_stoichiometry_MASTER.csv")

austraits_leaf_stoich_tib<-as_tibble(raw_data)

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


#outlier:Corymbia calophylla which is Myrtaceae Corymbia with 477 samples
ggplot(data = subset(species_count_tib, Freq <= 200), aes(x = reorder(species_binom, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.5, fill="darkgreen") +
  ggtitle("Number of Samples per Species, n < 200") +
  coord_flip()
  
n = 200
ggplot(data = subset(species_count_tib, Freq <= n), aes(x = reorder(species_binom, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.5, fill="darkgreen") +
  ggtitle("Number of Samples per Species, n <=", n) +
  coord_flip() +
  theme(
    axis.text.y = element_text(size = 2), # Adjust x-axis text size
    axis.title.y = element_text(size = 5)) # Adjust x-axis title size)

#next steps: where are the most samples, visualize frequencies over 10-50
#geographical distributions, are these all taken from the same place?

m = 10
ggplot(data = subset(species_count_tib, Freq > m), aes(x = reorder(species_binom, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.5, fill="darkgreen") +
  ggtitle(paste("Number of Samples per Species, Freq >", m)) +
  coord_flip() 
#extract list of most species with the most samples, somehow see if they are close geographically



#summary data
#10464 samples
#1421 species
#5.175352 average number of samples without main outlier


################## Leaf Nutrient Concentrations ###################

#take at look at warning, are there missing values - yes where there is NA
#nitrogen has no NA
#visualize these individually so there are no missing values

ggplot(data = austraits_leaf_stoich_tib) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_P_per_dry_mass))+ 
  ggtitle("Leaf N vs Leaf P")

ggplot(data = austraits_leaf_stoich_tib) + 
  geom_point(mapping = aes(x = leaf_P_per_dry_mass, y = leaf_C_per_dry_mass))+ 
  ggtitle("Leaf P vs Leaf C")

ggplot(data = austraits_leaf_stoich_tib) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_C_per_dry_mass))+ 
  ggtitle("Leaf N vs Leaf C")

#idea: faceting by family things like leaf N, to create summaries


################## Geographical Distribution ###################
#particularly of most species with highest sample frequency - freq > 50

#create tibble of just species geo data
species_geo_data <- tibble(
  species_binom = austraits_leaf_stoich_tib$species_binom,
  lat = austraits_leaf_stoich_tib$lat_deg,
  long = austraits_leaf_stoich_tib$long_deg,
)

#better way: include frequency in this tibble, avoids all problems down the line
species_geo_data_freq <- species_geo_data %>%
  group_by(species_binom) %>%
  mutate(frequency = n()) %>%
  ungroup() %>%
  select(species_binom, lat, long, frequency)

australia_map <- map_data("world", region = "Australia")

# Plot all species on the map
ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = species_geo_data, aes(x = long, y = lat), color = "darkgreen", size = 1) +
  labs(title = "All Species Locations", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )

#make tibble of just species with more than 50 samples with geo data
species_over_50 <- filter(species_geo_data_freq, frequency > 50)
#plot only these
ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = species_over_50, aes(x = long, y = lat), color = "darkgreen", size = 1) +
  labs(title = "Species Frequency > 50", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )

#now change colors (should be 9 different colors)
ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black")+
  geom_point(data = species_over_50, aes(x = long, y = lat, color = species_binom), shape = 3, size = 2) +
  scale_color_discrete() +
  labs(title = "Species Frequency > 50", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )



