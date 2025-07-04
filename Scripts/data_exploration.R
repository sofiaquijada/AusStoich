library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(httpgd)
library(languageserver)
library(lintr)
library(hypervolume)
library(rgl)

httpgd::hgd() #VS code plot viewer
hgd_browse()

aus_data

#------------------------Environmental Data Variation---------------------------
summary(aus_data) # look at quartiles, where most data lies


australia_map <- map_data("world", region = "Australia")
australia_map <- geom_polygon(data = australia_map, aes(x = long, y = lat,
                                                        group = group),
                              fill = "lightgray", color = "black")


env_data <- aus_data %>% select(
  lat_deg, long_deg,
  SN_total_0_30, SP_total_0_30, SOC_total_0_30,
  CEC_total_0_30, AP_total_0_30,
  NPP, MAT, PPT, AET,
  precipitation_seasonality, temp_seasonality) %>%
  #long format easier for facet wrapping
  pivot_longer(
    cols = -c(lat_deg, long_deg), names_to = "variable", values_to = "value")


#total observations on map
ggplot() +
  australia_map +
  geom_point(data = aus_data, aes(x = long_deg, y = lat_deg)) +
  theme_minimal() +
  labs(title = "Row Observations")

#look at categorical variables
#woodiness, life history, fixation, myc_type
ggplot() +
  australia_map +
  geom_point(data = aus_data, aes(x = long_deg, y = lat_deg,
                                  color = putative_BNF)) +
  theme_minimal()

#env variables on map
ggplot() + australia_map +
  geom_point(data = env_data,
             aes(x = long_deg, y = lat_deg,
                 size = value,
                 color = variable), alpha = 0.7) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  theme(legend.position = "right")
#not really comparable


#individual env variables mapped + visualized

#Options:
#SN_total_0_30, SP_total_0_30, SOC_total_0_30,
#CEC_total_0_30, AP_total_0_30,
#NPP, MAT, PPT, AET,
#precipitation_seasonality, temp_seasonality

ggplot() + australia_map +
  geom_point(data = env_data %>% filter(variable == "SN_total_0_30"),
             aes(x = long_deg, y = lat_deg,
                 size = value,
                 color = variable), alpha = 0.3) +
  theme_minimal() +
  theme(legend.position = "right")

#how are our values spread?
ggplot(data = aus_data, mapping = aes(x = temp_seasonality)) +
  geom_bar(fill = "salmon", width = 0.5) +
  theme_minimal()

ggplot(data = aus_data, mapping = aes(x = temp_seasonality)) +
  geom_histogram(fill = "salmon") +
  theme_minimal()


as.data.frame(aus_data)
a <- as.data.frame(aus_data)
env <- a %>% dplyr::select(SN_total_0_30, SP_total_0_30, SOC_total_0_30,
  CEC_total_0_30, AP_total_0_30,
  NPP, MAT, PPT, AET,
  precipitation_seasonality, temp_seasonality)

diag(solve(cor(env))) #look at highest ones
X <- cbind(rnorm(100), 1:100, 1:100+rnorm(100)); diag(solve(cor(X)))
#first low value, other ones will have high
#all vifs = 1 in perfect world (perfect indep variables)

cor(env)
library(corrplot)
corrplot(cor(env))

#--------------------------Leaf Nutrient Concentrations-------------------------

ggplot(data = aus_data) +
  geom_histogram(mapping = aes(x = ln_NP_ratio)) +
  theme_minimal()

#using average nutrient df from phylogeny script, of all species
ggplot(data = avg_ausdata) +
  geom_histogram(mapping = aes(x = CV_P)) +
  theme_minimal()

#dot plots of nutrient by species, most abundant
#corymbia_calophylla, eucalyptus_tereticornis, eucalyptus_tetrodonta, corymbia_terminalis
#eucalyptus_miniata, eucalyptus_macrorhyncha, acacia_aneura
#will remove species that have less than or equal to 62 entries
#to access 7 most common species
#View(as.data.frame(table(aus_data$species_binom)))
pruned <- prune_ausdata(aus_data, 62)

#doing it by dplyr better than pruning then plotting
ggplot(data = aus_data %>%
         group_by(species_binom) %>%
         filter(n() > 62) %>%
         ungroup(),
       aes(x = species_binom, y = leaf_N_per_dry_mass)) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Leaf Nitrogen Concentrations for Most Common Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#want to add column of entire variability of nutrient
pruned <- prune_ausdata(aus_data, 62)

complete <- aus_data %>% mutate(species_binom = "All Species")
combined <- bind_rows(pruned, complete)
combined$species_binom <- factor(combined$species_binom, 
      levels = c(unique(pruned$species_binom), "All Species"))

#success! can automate later... 
ggplot(combined, aes(x = species_binom, y = leaf_N_per_dry_mass)) +
  geom_jitter(width = 0.2, height = 0, size = 1, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Leaf Nitrogen Concentrations",
       x = "Species",
       y = "Leaf Nitrogen Concentration (per dry mass)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#box plot by myc type & PFTs
#in poster visualizations

#ratios by major family

#geometric average per family
#recall arithmetic mean of logged values = geometric mean
aus_data %>%
  group_by(family) %>%
  filter(n() > 99) %>%
  summarise(mean_CN = mean(ln_CN_ratio, na.rm = TRUE)) %>%
  ggplot(aes(x = family, y = mean_CN)) +
  geom_col() +
  theme_minimal()

#jitter plot
aus_data %>%
  group_by(family) %>%
  filter(n() > 99) %>%
  ungroup() %>%
  ggplot(aes(x = family, y = CN_ratio)) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4, fatten = 0, color = "black") +
  theme_minimal() +
  labs(title = "C:N Ratios per Family (n > 99)",
       x = "Family", y = "C:N Ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#how many P and N values below 1?
#only one N
#3051 P below one


#--------------------------Trait Env Relationships-------------------------

summary(glm(log(leaf_N_per_dry_mass) ~ SN_total_0_30 + PPT,
            data = aus_data, family = "gaussian"))

#linear regressions by species
#corymbia_calophylla, eucalyptus_tereticornis, eucalyptus_tetrodonta, corymbia_terminalis
#eucalyptus_miniata, eucalyptus_macrorhyncha, acacia_aneura
#will remove species that have less than or equal to 62 entries
#to access 7 most common species

#linear regression of top 7 species
#leaf N by SN

lm <- lm(log(leaf_N_per_dry_mass) ~ SN_total_0_30, data = aus_data)
plot(lm)  #ok

#all data
ggplot(aus_data, aes(x = AP_total_0_30, y = (leaf_P_per_dry_mass))) +
  geom_point(alpha = 0.6) +  # Scatter plot of the data points
  geom_smooth(method = "lm", col = "blue") +
  theme_classic() +
  labs(title = "log(leaf_N_per_dry_mass) ~ AP_total_0_30",
       x = "AP_total_0_30",
       y = "(leaf_P_per_dry_mass)")

pruned <- prune_ausdata(aus_data, 62)

#save me why does species level curve show no all-encompassing relationship
ggplot(pruned, aes(x = AP_total_0_30, y = (leaf_P_per_dry_mass), color = species_binom)) +
  geom_point(alpha = 0.6) +
  #geom_smooth(method = "lm", se = FALSE) +  #just confuses things more
  theme_minimal() +
 #labs(title = "log(leaf_N_per_dry_mass) ~ SN_total_0_30",
      # x = "SN_total_0_30",
      # y = "log(leaf_N_per_dry_mass)",
      # color = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#3 most common genera: eucalyptus, corymbia, acacia, banksia
#added banksia as proteaceae representative
genera_pruned <- aus_data %>%
  filter(genus %in% c("Eucalyptus", "Corymbia", "Acacia", "Banksia"))

  ggplot(genera_pruned, aes(x = AP_total_0_30, y = log(leaf_P_per_dry_mass), color = genus)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#3 most common families: myrtaceae, fabaceae, proteaceae
family_pruned <- aus_data %>%
  filter(family %in% c("Myrtaceae", "Fabaceae", "Proteaceae"))
  ggplot(family_pruned, aes(x = SN_total_0_30, y = log(leaf_N_per_dry_mass), color = family)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
#variation along environmental gradients
ggplot(aus_data, aes(x = SN_total_0_30, y = sd_ln_NP)) +
  geom_point(alpha = 0.6) +
  #geom_smooth(method = "lm", col = "blue") +
  theme_classic()

#-----------------------Species Frequency + Spread------------------------------
species <- as.data.frame(table(aus_data$species_binom))  %>%
  arrange(desc(Freq)) %>%
  rename(species_binom = Var1)

#basic
ggplot(data = aus_data, mapping = aes(x = species_binom)) +
  geom_bar() +
  theme_minimal()

#how many of each species
ggplot(data = species, mapping = aes(x = reorder(species_binom, -Freq),
                                     y = Freq)) +
  geom_col() +
  labs(x = "Species") +
  coord_flip() +
  theme_minimal()

ggplot(data = subset(species, Freq > 30),
       mapping = aes(x = reorder(species_binom, -Freq), y = Freq,
                     fill = species_binom)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Species") +
  coord_flip() +
  theme_minimal()

#species frequency table with associated location for plotting
species_geo <- tibble(
  species_binom = aus_data$species_binom,
  lat = aus_data$lat_deg,
  long = aus_data$long_deg) %>%
  group_by(species_binom) %>%
  mutate(frequency = n()) %>%
  ungroup() %>%
  select(species_binom, lat, long, frequency) %>%
  distinct() %>% #to remove duplicate rows
  arrange(desc(frequency))
#here, frequency = #of observations in original df

#aus_data = 7818 observations
#species_geo = 2982 unique species observations


#species plot on australia map
ggplot() + australia_map +
  geom_point(data = subset(species_geo, frequency > 50), mapping = aes(x = long, y = lat,
             color = species_binom), size = 2) +
  scale_color_discrete() +
  theme_minimal()

#specific species
ggplot() + australia_map +
  geom_point(data = subset(species_geo, species_binom == "Mesomelaena_pseudostygia"),
  mapping = aes(x = long, y = lat, color = species_binom), size = 2) +
  scale_color_discrete() +
  theme_minimal()


#number of species per observation number, with associated list
species_observations <- species %>%
  group_by(Freq) %>%
  summarize(
    species_count = n(),
    species_list = list(toString(species_binom))
  ) %>%
  ungroup()


#how many species per number of observations
ggplot(data = species_observations) +
  geom_col(mapping = aes(x = Freq, y = species_count)) +
  theme_minimal()
#most species just have one observation

#subsetting by frequency
ggplot(data = subset(species_observations, Freq < 50)) +
  geom_col(mapping = aes(x = Freq, y = species_count)) +
  theme_minimal()


#---------genus
genus <- as.data.frame(table(aus_data$genus)) %>%
  arrange(desc(Freq)) %>%
  rename(genus = Var1)

#similar to species observations
#doesn't have an use (yet)
#82 genera have only one entry
genus_obs <- aus_data %>%
      count(genus) %>%
      arrange(desc(n)) %>%
      group_by(n) %>%
      summarize(
        genus_count = n(),
        genera_list = list(toString(genus))
      ) %>%
      ungroup()

ggplot(data = genus, mapping = aes(x = reorder(genus, -Freq),
                                     y = Freq)) +
  geom_col() +
  labs(x = "Genus") +
  coord_flip() +
  theme_minimal()

ggplot(data = subset(genus, Freq > 60),
       mapping = aes(x = reorder(genus, -Freq), y = Freq,
                     fill = genus)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Genus") +
  coord_flip() +
  theme_minimal()


#--------family
family <- as.data.frame(table(aus_data$family)) %>%
  arrange(desc(Freq)) %>%
  rename(family = Var1)


ggplot(data = family, mapping = aes(x = reorder(family, -Freq),
                                   y = Freq)) +
  geom_col() +
  labs(x = "Family") +
  coord_flip() +
  theme_minimal()

family_sp <- aus_data %>%
  group_by(family) %>%
  summarise(species = paste(unique(species_binom), collapse = ", ")) %>%
  arrange(family)

family_sp <- aus_data %>%
  filter(family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")) %>%
  select(family, species_binom) %>%
  distinct() %>%
  mutate(family = factor(family, levels = c("Myrtaceae", "Fabaceae", "Proteaceae"))) %>%
  arrange(family)

View(family_sp)


#--------------------------------Species Identity-------------------------------

#there are 5 gymnosperm families
gymnosperm_data <- aus_data %>%
  filter(family %in% c("Podocarpaceae", "Cupressaceae",
                       "Araucariaceae", "Zamiaceae", "Cycadaceae"))


ausdata_no_gymn<- aus_data%>% 
  filter(!family %in% c("Podocarpaceae", "Cupressaceae",
                        "Araucariaceae", "Zamiaceae", "Cycadaceae"))


#--------------------------------Missing Data-----------------------------------

#NAs stored as string for visualization purposes
na_data <- aus_data %>%
  mutate(across(c(leaf_P_per_dry_mass,leaf_C_per_dry_mass,
                  NP_ratio, CN_ratio, CP_ratio), as.character)) %>% 
  replace_na(list(leaf_N_per_dry_mass= "NA", leaf_P_per_dry_mass = "NA",
                  leaf_C_per_dry_mass = "NA",NP_ratio = "NA",
                  CN_ratio = "NA", CP_ratio = "NA"))
#entire columns stored as characters

na_data <- na_data %>%
  filter(leaf_P_per_dry_mass == "NA" | leaf_C_per_dry_mass == "NA" |
           NP_ratio == "NA" | CN_ratio == "NA" | CP_ratio == "NA")
#6508 entries
#meaning that 83% of rows have at least one leaf nutrient NA


ggplot() +
  australia_map +
  geom_point(data = na_data, aes(x = long_deg, y = lat_deg)) +
  theme_minimal() +
  labs(title = "Missing Data")
#missing data is spread evenly across Australia


#---------families and genera with missing data
ggplot(data = na_data, mapping = aes(x = family)) +
  geom_bar(fill = "darkgreen") +
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Missing Data")

ggplot(data = na_data, mapping = aes(x = reorder(family, -after_stat(count)))) +
  geom_bar(fill = "darkgreen") + 
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Missing Data")

ggplot(data = na_data, mapping = aes(x = reorder(family, -after_stat(count)))) +
  geom_bar(fill = "darkgreen") + 
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Missing Data")

ggplot(data = na_data, mapping = aes(x = genus)) +
  geom_bar(fill = "darkgreen") + 
  theme_minimal() +
  coord_flip() + 
  theme(axis.text.y = element_text(size = 2)) +
  labs(title = "Missing Data")


#do species frequency stuff again here
length(unique(na_data$family)) #120
length(unique(na_data$genus)) #395
length(unique(na_data$species_binom)) #1337

#--------counts of missing data
na_species <- as.data.frame(table(na_data$species_binom))
na_species <- na_species %>%
  arrange(desc(Freq))

na_genus <- as.data.frame(table(na_data$genus))
na_genus <- na_genus %>%
  arrange(desc(Freq))

na_family <- as.data.frame(table(na_data$family))%>%
  arrange(desc(Freq))


ggplot(data = subset(species, Freq > 30),
       mapping = aes(x = reorder(Var1, -Freq), y = Freq,
                     fill = Var1)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Species") +
  coord_flip() +
  theme_minimal() 


ggplot(data = subset(na_genus, Freq > 50), mapping = aes(x = reorder(Var1, -Freq),
                                   y = Freq, fill = Var1)) +
  geom_col() +
  scale_fill_discrete() +
  labs(x = "Genera with NAs") +
  coord_flip() +
  theme_minimal()

ggplot(data = subset(na_genus, Freq < 100), mapping = aes(x = reorder(Var1, -Freq),
                                                         y = Freq)) +
  geom_col() +
  labs(x = "Genera with NAs < 100") +
  coord_flip() +
  theme_minimal()

#everything before is just the presence of ONE NA in any leaf nutrient column
#from excel, 2083 rows have BOTH no C and no P values

two_na_data <- na_data %>%
  filter(leaf_C_per_dry_mass == "NA" & leaf_P_per_dry_mass == "NA")

#for either one or the other:
one_na_data <- na_data %>%
  filter(!(leaf_C_per_dry_mass == "NA" & leaf_P_per_dry_mass == "NA"))

ggplot() +
  australia_map +
  geom_point(data = two_na_data, aes(x = long_deg, y = lat_deg)) +
  theme_minimal()+
  labs(title = "Missing C and P values")

#------------Missing data densities

#base of all leaf N observations
ggplot(data = aus_data, mapping = aes(x = leaf_N_per_dry_mass)) +
      geom_density() +
      theme_minimal()

#try katies way, thank you katie :"))))))

#specify colors
color_palette <- c("NA rows" = "lightgray", "All data" = "lightgreen")

#density plot of missing leaf concentration data
#using (complete) leaf N to compare rows with NAs for P and C
ggplot() +
  geom_density(data = aus_data,
               mapping = aes(x = leaf_N_per_dry_mass,
                             fill = "All data", alpha = 0.5)) +
  geom_density(data = na_data,
               mapping = aes(x = leaf_N_per_dry_mass,
                             fill = "NA rows", alpha = 0.5)) +
  labs(title = "Leaf N observations with Missing Leaf P and/or Leaf C",
       x = "leaf N", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = color_palette, name = " ") +
  theme(legend.position = c(0.83, 0.86))
  

#Density plot of missing leaf data across environmental variables
#do for all environmental variables


#Options:
#SN_total_0_30, SP_total_0_30, SOC_total_0_30,
#CEC_total_0_30, AP_total_0_30,
#NPP, MAT, PPT, AET,
#precipitation_seasonality, temp_seasonality

ggplot() +
  geom_density(data = aus_data,
               mapping = aes(x = SN_total_0_30,
                             fill = "All data", alpha = 0.5)) +
  geom_density(data = na_data,
               mapping = aes(x = SN_total_0_30,
                             fill = "NA rows", alpha = 0.5)) +
  labs(title = "Observations with Missing Leaf P and/or Leaf C",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = color_palette, name = " ") +
  theme(legend.position = c(0.83, 0.86))