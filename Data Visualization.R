install.packages("tidyverse")
install.packages("cowplot")
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(cowplot)
library(patchwork)

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

m = 50
ggplot(data = subset((species_count_tib), Freq > m), aes(x = reorder(species_binom, -Freq), y = Freq)) +
  geom_bar(stat="identity", width=0.5, fill="darkgreen") +
  ggtitle(paste("Number of Samples per Species, Freq >", m)) +
  coord_flip() 


#summary data
#10464 samples
#1421 species
#5.175352 average number of samples without main outlier


# How many woody/non-woody, nitrogen fixers/non fixers 
################## Amount Types ##################

#woodiness
ggplot(data = leaf_data, aes(x = woodiness)) +
  geom_bar(width = 0.5, fill = "darkgreen") +
  ggtitle("Count of Woody vs Herbaceous Species") +
  ylab("Count") +
  theme_minimal()

#fixers
ggplot(data = leaf_data, aes(x = factor(putative_BNF))) +
  geom_bar(width = 0.2, fill = "darkgreen") +
  ggtitle("Count of Nitrogen Fixers") +
  ylab("Count") +
  scale_x_discrete(name = "Putative BNF", 
  labels = c("0" = "Non-Fixers", "1" = "Fixers")) +
  theme_minimal()

ggplot(data = leaf_data, aes(x = reclass_life_history)) +
  geom_bar(width = 0.5, fill = "darkgreen") +
  ggtitle("Count of Woody vs Herbaceous Species") +
  ylab("Count") +
  theme_minimal()

#life history

################## Leaf Nutrient Concentrations ###################

#take at look at warning, are there missing values - yes where there is NA
#nitrogen has no NA

N_vs_P <- ggplot(data = austraits_leaf_stoich_tib) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_P_per_dry_mass))+ 
  ggtitle("Leaf N vs Leaf P")

N_vs_C <-ggplot(data = austraits_leaf_stoich_tib) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_C_per_dry_mass))+ 
  ggtitle("Leaf N vs Leaf C")

P_vs_C <-ggplot(data = austraits_leaf_stoich_tib) + 
  geom_point(mapping = aes(x = leaf_P_per_dry_mass, y = leaf_C_per_dry_mass))+ 
  ggtitle("Leaf P vs Leaf C")

nutrient_plots <- plot_grid(
  N_vs_P,
  N_vs_C,
  P_vs_C)

#do the same, but only with species w freq over 50 
#create tibble with species count

leaf_data <- austraits_leaf_stoich_tib %>%
  add_count(species_binom, name = "frequency") #tibble that includes frequency 

n = 50
N_vs_P_over_50 <- ggplot(data = leaf_data %>% filter(frequency > n)) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_P_per_dry_mass)) + 
  ggtitle(paste("Leaf N vs Leaf P, Freq >", n))

N_vs_C_over_50 <- ggplot(data = leaf_data %>% filter(frequency > n)) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_C_per_dry_mass)) + 
  ggtitle(paste("Leaf N vs Leaf C, Freq >", n))

P_vs_C_over_50 <- ggplot(data = leaf_data %>% filter(frequency > n)) + 
  geom_point(mapping = aes(x = leaf_P_per_dry_mass, y = leaf_C_per_dry_mass)) + 
  ggtitle(paste("Leaf P vs Leaf C, Freq >", n))

nutrient_plots_over_50 <- plot_grid(
  N_vs_P_over_50,
  N_vs_C_over_50,
  P_vs_C_over_50)

n = 30
m = 50
N_vs_P_over_30_under_50 <- ggplot(data = leaf_data %>% 
  filter(frequency > n & frequency < m)) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_P_per_dry_mass)) + 
  ggtitle(paste("Leaf N vs Leaf P,",n,"< Freq <", m))

N_vs_C_over_30_under_50 <- ggplot(data = leaf_data %>% filter(frequency > n)) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_C_per_dry_mass)) + 
  ggtitle(paste("Leaf N vs Leaf C,",n,"< Freq <", m))

P_vs_C_over_30_under_50 <- ggplot(data = leaf_data %>% filter(frequency > n)) + 
  geom_point(mapping = aes(x = leaf_P_per_dry_mass, y = leaf_C_per_dry_mass)) + 
  ggtitle(paste("Leaf P vs Leaf C,",n,"< Freq <", m))

nutrient_plots_over_30_under_50 <- plot_grid(
  N_vs_P_over_30_under_50,
  N_vs_C_over_30_under_50,
  P_vs_C_over_30_under_50)

#next: partition these into the 8 biggest families



#and amount of leaf N (most interesting: n fixers or not as x and y, 
#and three seperate lines for N C and P amounts)

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
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group),
  fill = "lightgray", color = "black") +
  geom_point(data = species_geo_data, aes(x = long, y = lat), color = "darkgreen", size = 1) +
  labs(title = "All Species Locations", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )

#make tibble of just species with more than 50 samples with geo data
species_over_50_geo <- filter(species_geo_data_freq, frequency > 50)
#plot only these
ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group),
  fill = "lightgray", color = "black") +
  geom_point(data = species_over_50_geo, aes(x = long, y = lat), color = "darkgreen", size = 1) +
  labs(title = "Species Frequency > 50", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )

#now change colors (should be 9 different colors)
australia_over_50_map <- ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group),
  fill = "lightgray", color = "black")+
  geom_point(data = species_over_50_geo, aes(x = long, y = lat, color = species_binom),
  shape = 3, size = 2) +
  scale_color_discrete() +
  labs(title = "Species Frequency > 50", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  ) 

m = 50
australia_over_50_bar <- ggplot(
  data = subset(species_count_tib, Freq > m),
  aes(x = reorder(species_binom, -Freq), y = Freq, fill = species_binom)) +
  geom_bar(stat="identity", width=0.5) +
  scale_fill_discrete() +
  ggtitle(paste("Number of Samples per Species, Freq >", m)) +
  coord_flip()
#color coded bar graph

combined_plot <- plot_grid(australia_over_50_bar, australia_over_50_map,
                  labels = "AUTO")

#Plot Freq over 30 and exclude 8 most common 

#make tibble of just species with more than 30 samples with geo data
#exclude frequency over 50 
species_over_30_under_50 <- filter(species_geo_data_freq, frequency > 30, frequency < 50)
#plot only these

australia_over_30_map <- ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "black") +
  geom_point(data = species_over_30_under_50, aes(x = long, y = lat, color = species_binom),
  shape = 3, size = 2) +
  labs(title = "30 < Species Frequency < 50", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )

#bar plot of over 30 under 50
australia_over_30_bar <- ggplot(
  data = subset(species_count_tib, Freq > 30 & Freq < 50),
  aes(x = reorder(species_binom, -Freq), y = Freq, fill = species_binom)) +
  geom_bar(stat="identity", width=0.5) +
  scale_fill_discrete() +
  ggtitle(paste("Number of Samples per Species, 30 < Freq < 50")) +
  coord_flip()

#join two plots together 
plot_grid(
  australia_over_30_bar + theme(legend.position = "none") + theme(plot.title = element_text(size = 8)), 
  australia_over_30_map, 
  labels = "AUTO", 
  rel_widths = c(1, 2)
)


          