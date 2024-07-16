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

#---------------- Function Definitions ----------------

#make a function that returns a plot
#i guess a different one for every type of plot? 

#make a function that stitches plots together 
#have infinite inputs? will look into this 


#------------------------------------------------------

#---------------- Number of Samples per Species ----------------

#alternate method to count sample number of species:
#count <- austraits_leaf_stoich_tib %>% count(austraits_leaf_stoich_tib$species_binom)

species_binom <- raw_data$species_binom
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

#------------------------------------------------------


# How many woody/non-woody, nitrogen fixers/non fixers 
#---------------- Amount Types ----------------

#woodiness
woodiness_count_bar <- ggplot(data = leaf_data, aes(x = woodiness)) +
  geom_bar(width = 0.5, fill = "darkgreen") +
  ggtitle("Count of Woody vs Herbaceous Species") +
  ylab("Count") +
  theme_minimal()

#fixers
fixers_count_bar <- ggplot(data = leaf_data, aes(x = factor(putative_BNF))) +
  geom_bar(width = 0.2, fill = "darkgreen") +
  ggtitle("Count of Nitrogen Fixers") +
  ylab("Count") +
  scale_x_discrete(name = "Putative BNF", 
  labels = c("0" = "Non-Fixers", "1" = "Fixers")) +
  theme_minimal()

#life history
life_history_count_bar <- ggplot(data = leaf_data, aes(x = reclass_life_history)) +
  geom_bar(width = 0.5, fill = "darkgreen") +
  ggtitle("Life History") +
  ylab("Count") +
  theme_minimal()

#myc type
myc_type_count_bar <- ggplot(data = leaf_data, aes(x = myc_type)) +
  geom_bar(width = 0.5, fill = "darkgreen") +
  ggtitle("Myc Type") +
  ylab("Count") +
  theme_minimal()

categorical_counts <- plot_grid(
  woodiness_count_bar,
  fixers_count_bar,
  life_history_count_bar,
  myc_type_count_bar)


#---------------- Leaf Nutrient Concentrations ----------------

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

N_vs_C_over_30_under_50 <- ggplot(data = leaf_data %>%
  filter(frequency > n & frequency < m)) + 
  geom_point(mapping = aes(x = leaf_N_per_dry_mass, y = leaf_C_per_dry_mass)) + 
  ggtitle(paste("Leaf N vs Leaf C,",n,"< Freq <", m))

P_vs_C_over_30_under_50 <- ggplot(data = leaf_data %>% 
  filter(frequency > n & frequency < m)) + 
  geom_point(mapping = aes(x = leaf_P_per_dry_mass, y = leaf_C_per_dry_mass)) + 
  ggtitle(paste("Leaf P vs Leaf C,",n,"< Freq <", m))

nutrient_plots_over_30_under_50 <- plot_grid(
  N_vs_P_over_30_under_50,
  N_vs_C_over_30_under_50,
  P_vs_C_over_30_under_50)

#next: partition these into the 8 biggest families

#filtered by categorical variables
#create data frame with all categorical variables + concentration values
leaf_concentration_data <- data.frame(
  unique_ID = leaf_data$Unique_ID,
  woodiness = leaf_data$woodiness,
  reclass_life_history = leaf_data$reclass_life_history,
  putative_BNF = leaf_data$putative_BNF,
  myc_type = leaf_data$myc_type,
  concentration = c(leaf_data$leaf_N_per_dry_mass, leaf_data$leaf_P_per_dry_mass,
                    leaf_data$leaf_C_per_dry_mass),
  nutrient = factor(rep(c("N", "P", "C"), each = nrow(leaf_data)))
  )

#all three nutrients by categorical variables
nutrients_by_woodiness <- ggplot(leaf_concentration_data, aes(x = factor(woodiness), y = concentration, fill = nutrient)) + 
  geom_boxplot(position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow", "C" = "lightgreen")) +
  labs(x = "Woodiness", y = "Concentration", fill = "Nutrient") +
  ggtitle("Nutrient Concentrations by Woodiness") +
  theme_minimal()

nutrients_by_life_history <- ggplot(leaf_concentration_data, aes(x = factor(reclass_life_history), y = concentration, fill = nutrient)) + 
  geom_boxplot(position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow", "C" = "lightgreen")) +
  labs(x = "Life History", y = "Concentration", fill = "Nutrient") +
  ggtitle("Nutrient Concentrations by Life History") +
  theme_minimal()

nutrients_by_fixer_type <- ggplot(leaf_concentration_data, aes(x = factor(putative_BNF), y = concentration, fill = nutrient)) + 
  geom_boxplot(position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow", "C" = "lightgreen")) +
  labs(x = "Nitrogen Fixers", y = "Concentration", fill = "Nutrient") +
  scale_x_discrete(labels = c("0" = "Non-Fixer", "1" = "Fixer")) +
  ggtitle("Nutrient Concentrations by Fixer Type") +
  theme_minimal()

nutrients_by_myc_type <- ggplot(leaf_concentration_data, aes(x = factor(myc_type), y = concentration, fill = nutrient)) + 
  geom_boxplot(position = position_dodge(width = 1)) +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow", "C" = "lightgreen")) +
  labs(x = "Myc Type", y = "Concentration", fill = "Nutrient") +
  ggtitle("Nutrient Concentrations by Myc Type") +
  theme_minimal()

plot_grid(
  nutrients_by_woodiness,
  nutrients_by_life_history,
  nutrients_by_fixer_type,
  nutrients_by_myc_type)

#N and P data only
leaf_N_and_P_data <- subset(leaf_concentration_data, nutrient %in% c("N", "P"))

N_P_woodiness <- ggplot(leaf_N_and_P_data, aes(x = factor(woodiness), y = concentration, fill = nutrient)) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow")) +
  labs(x = "Woodiness", y = "Concentration", fill = "Nutrient") +
  ggtitle("N and P Concentrations by Woodiness") +
  theme_minimal()

N_P_life_history <- ggplot(leaf_N_and_P_data, aes(x = factor(reclass_life_history), y = concentration, fill = nutrient)) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow")) +
  labs(x = "Life History", y = "Concentration", fill = "Nutrient") +
  ggtitle("N and P Concentrations by Life History") +
  theme_minimal()

N_P_fixer_type <- ggplot(leaf_N_and_P_data, aes(x = factor(putative_BNF), y = concentration, fill = nutrient)) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow")) +
  labs(x = "Nitrogen Fixers", y = "Concentration", fill = "Nutrient") +
  scale_x_discrete(labels = c("0" = "Non-Fixer", "1" = "Fixer")) +
  ggtitle("N and P Concentrations by Fixer Type") +
  theme_minimal()

N_P_myc_type <- ggplot(leaf_N_and_P_data, aes(x = factor(myc_type), y = concentration, fill = nutrient)) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow")) +
  labs(x = "Myc type", y = "Concentration", fill = "Nutrient") +
  ggtitle("N and P Concentrations by Myc Type") +
  theme_minimal()

plot_grid(
  N_P_woodiness,
  N_P_life_history,
  N_P_fixer_type,
  N_P_myc_type
  )

leaf_C_data <- subset(leaf_concentration_data, nutrient %in% c("C"))
leaf_N_data <- subset(leaf_concentration_data, nutrient %in% c("N"))
leaf_P_data <- subset(leaf_concentration_data, nutrient %in% c("P"))

ggplot(leaf_C_data, aes(x = factor(woodiness), y = concentration, fill = nutrient)) +
  geom_boxplot() +
  scale_fill_manual(values = c("C" = "lightgreen")) +
  labs(x = "Woodiness", y = "Concentration", fill = "Nutrient") +
  ggtitle("C concentrations by Woodiness") +
  theme_minimal()

ggplot(leaf_concentration_data, aes(x = factor(woodiness), y = concentration, fill = nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow", "C" = "lightgreen")) +
  facet_wrap(~nutrient)
#this isnt great 
   
ggplot(leaf_concentration_data, aes(x=factor(woodiness), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("N" = "pink", "P" = "lightyellow", "C" = "lightgreen")) +
  facet_wrap(~woodiness, scale="free")
# better way to do what i did above. whatever

plot_1 <- ggplot(leaf_C_data, aes(x=factor(woodiness), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("C" = "lightgreen")) 

plot_2 <- ggplot(leaf_N_data, aes(x=factor(woodiness), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("N" = "pink"))

plot_3 <- ggplot(leaf_P_data, aes(x=factor(woodiness), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("P" = "lightyellow"))

plot_4 <- ggplot(leaf_C_data, aes(x=factor(reclass_life_history), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("C" = "lightgreen")) 

plot_5 <- ggplot(leaf_N_data, aes(x=factor(reclass_life_history), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("N" = "pink")) 

plot_6 <- ggplot(leaf_P_data, aes(x=factor(reclass_life_history), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("P" = "lightyellow")) 

plot_7 <- ggplot(leaf_C_data, aes(x=factor(myc_type), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("C" = "lightgreen"))

plot_8 <- ggplot(leaf_N_data, aes(x=factor(myc_type), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("N" = "pink"))

plot_9 <- ggplot(leaf_P_data, aes(x=factor(myc_type), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("P" = "lightyellow")) 

plot_10 <- ggplot(leaf_C_data, aes(x=factor(putative_BNF), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("C" = "lightgreen")) +
  scale_x_discrete(labels = c("0" = "Non-Fixer", "1" = "Fixer")) 

plot_11 <- ggplot(leaf_N_data, aes(x=factor(putative_BNF), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("N" = "pink")) +
  scale_x_discrete(labels = c("0" = "Non-Fixer", "1" = "Fixer")) 

plot_12 <- ggplot(leaf_P_data, aes(x=factor(putative_BNF), y=concentration, fill=nutrient)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("P" = "lightyellow")) +
  scale_x_discrete(labels = c("0" = "Non-Fixer", "1" = "Fixer")) 
#will rewrite using functions

#------------------------------------------------------


#---------------- Geographical Distribution ----------------
#particularly of most species with highest sample frequency - freq > 50

#create tibble of just species geo data
species_geo_data <- tibble(
  species_binom = austraits_leaf_stoich_tib$species_binom,
  lat = austraits_leaf_stoich_tib$lat_deg,
  long = austraits_leaf_stoich_tib$long_deg
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
  size = 2) +
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
               fill = "lightgray", color = "black") + coord_quickmap()+
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
#rewrite using functions
#------------------------------------------------------


#---------------- Avg Nutrient Plots  ----------------
#create average nutrient dataframe but with ALL species
#avg nutrient & CV dataframe

#need to add counts column to this to properly normalize
#x: counts, y: concentration. see if normal, if not then plot transformed values

#leaf data has count built in already
all_species_nutr_data <- leaf_data[,c("species_binom", "leaf_N_per_dry_mass",
                             "leaf_P_per_dry_mass", "leaf_C_per_dry_mass",
                             "NP_ratio", "CN_ratio", "CP_ratio", 
                             "frequency")]
#replace NAs with 0 
all_species_nutr_data <- all_species_nutr_data %>%
  mutate_all(~replace(., is.na(.), 0))

#order alphabetically
all_species_nutr_data <- all_species_nutr_data[order(all_species_nutr_data$species_binom),]

#add coefficient of variation
all_species_nutr_data <- all_species_nutr_data %>%
  group_by(species_binom) %>%
  mutate(CV_N = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                             na.rm = TRUE),
         CV_P = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                             na.rm = TRUE),
         CV_C = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                             na.rm = TRUE))
#fill NA and NaN with 0                                                        
all_species_nutr_data <- all_species_nutr_data %>%
  mutate_all(~replace(., is.na(.), 0))

all_species_nutr_data <- all_species_nutr_data %>%
  mutate_all(~replace(.,is.nan(.), 0))

all_species_nutr_data<- aggregate(. ~ species_binom,
                                  data = all_species_nutr_data, FUN = mean)

#plot by species
ggplot(all_species_nutr_data, aes(x = species_binom, y = leaf_N_per_dry_mass)) + 
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(title = "All Species")+
  theme_minimal()

#plot by frequency - has to be point
#ggplot(all_species_nutr_data, aes(x = frequency, y = leaf_N_per_dry_mass)) + 
  #geom_point(stat = "identity", fill = "lightpink") +
  #labs(title = "All Species")+
  #theme_minimal()

ggplot(all_species_nutr_data, aes(x = leaf_N_per_dry_mass, y = frequency)) + 
  geom_point(stat = "identity", fill = "lightpink") +
  labs(title = "All Species")+
  theme_minimal()

#create transformed data
trns_all_sp_nut<- all_species_nutr_data%>% 
  mutate(
    ln_N = log(leaf_N_per_dry_mass),
    log_10_N = log10(leaf_N_per_dry_mass),
    sqrt_N = sqrt(leaf_N_per_dry_mass),
    ln_C = log(leaf_C_per_dry_mass),
    log_10_C = log10(leaf_C_per_dry_mass),
    sqrt_C = sqrt(leaf_C_per_dry_mass),
    ln_P = log(leaf_P_per_dry_mass),
    log_10_P = log10(leaf_P_per_dry_mass),
    sqrt_P = sqrt(leaf_P_per_dry_mass)
  )
trns_all_sp_nut[trns_all_sp_nut == "-Inf"] <- 0
trns_all_sp_nut[trns_all_sp_nut == "0"] <- NA

ggplot(trns_all_sp_nut, aes(x = log_10_N, y = frequency)) + 
  geom_point(stat = "identity") +
  labs(title = "All Species")+
  theme_minimal()



#---------------- Ratio Histograms  ----------------
#ensure they are normal 
#x = ratio, y = frequency 

#NP
#CN
#CP

ggplot(trns_all_sp_nut, aes(x = CP_ratio, y = frequency)) + 
  geom_point(stat = "identity") +
  labs(title = "All Species")+
  theme_minimal() #this is not the best approach, use geom_histogram()
#if geom_bar is used - a lot of points get cut for some reason

#make df with transformed data 
raw_nutrient_data <- austraits_leaf_stoich[,c("species_binom",
                      "leaf_N_per_dry_mass", "leaf_P_per_dry_mass",
                      "leaf_C_per_dry_mass", "NP_ratio", "CN_ratio",
                      "CP_ratio")]

nutrient_data <- raw_nutrient_data %>% 
  mutate(
    ln_N = log(leaf_N_per_dry_mass),
    log_10_N = log10(leaf_N_per_dry_mass),
    sqrt_N = sqrt(leaf_N_per_dry_mass),
    ln_C = log(leaf_C_per_dry_mass),
    log_10_C = log10(leaf_C_per_dry_mass),
    sqrt_C = sqrt(leaf_C_per_dry_mass),
    ln_P = log(leaf_P_per_dry_mass),
    log_10_P = log10(leaf_P_per_dry_mass),
    sqrt_P = sqrt(leaf_P_per_dry_mass)
  ) 

#sample histogram code
ggplot(nutrient_data, aes(x = leaf_N_per_dry_mass)) +
  geom_histogram(bins = 50) +
  labs(title = "All Species")
#bin width = bar width
#not too big, not to small 

#notes: sqrtx is probably best since all P values are < 1

ggplot(nutrient_data, aes(x = CP_ratio)) +
  geom_histogram(bins = 100) +
  labs(title = "All Species")









          