library(rtry)
library(dplyr)
library(tidyr)
library(jsonlite)
library(curl)
library(data.table)
library(ggplot2)

setwd("/Users/sofiaquijada/Desktop/")
getwd()

try_data <- rtry_import(
                    input ="TRY data AusStoich/leaf_data.txt",
                    separator = "\t",
                    encoding = "Latin-1",
                    quote = "",
                    showOverview = TRUE)

#gives nice summary, observation counts per species
trydata_summary <- rtry_explore(try_data, AccSpeciesName, TraitName, TraitID, sortBy = AccSpeciesID)

#include exclusively mature individuals, leaves, and non-experimental

# 327 - experimental. Remove all experimental entries 
try_data <- try_data %>% filter(DataID != 327)
View(filter(try_data, DataID == 327))

# 413 - maturity information 
age <- filter(try_data, DataID == 413)
#need to prune to exclusively mature individuals, by StdValueStr
#so include StdValueStr = mature, Adult
try_data <- try_data %>%
  filter(!(DataID == 413 & !StdValueStr %in% c("Adult", "mature")))

# 1444 - leaf development
View(filter(try_data, DataID == 1444))
#only include mature leaves, by OrigValueStr
try_data <- try_data %>%
  filter(!(DataID == 1444 & !StdValueStr == "mature"))

# 1961 - leaf health
View(filter(try_data, DataID == 1961))
#they all seem healthy

trydata_pruned <- rtry_select_col(input = try_data, AccSpeciesName, ObservationID, ObsDataID,
               OrigUnitStr, OrigValueStr, TraitName, DataName, StdValue, UnitName, StdValueStr)

#only want lat longs + continent + region, and trait data in mg/g + ratios
#so keep only trait data in TraitName and columns in "DataName" that are latitude or long

keep <- c("Latitude", "Longitude", "Location Continent", "Location Region", 
          "Leaf nitrogen content per dry mass (Nmass)",
          "Leaf phosphorus content per dry mass (Pmass)",
          "Leaf carbon content per dry mass",
          "Leaf nitrogen/phosphorus (N/P) ratio", 
          "Leaf carbon/nitrogen (C/N) ratio",
          "Leaf carbon/phosphorus (C/P) ratio",
          "Location Country",
          "AccSpeciesName")


trydata_pruned <- filter(trydata_pruned, DataName%in%keep)
#ensure all plain trait data is in mg/g

#for dataname in leaf N, P or C, get rid of all entries whose units aren't in mg/g
#also in ratios, since they some are in g/g
trydata_pruned <- trydata_pruned %>% 
    filter(!(DataName %in% c("Leaf nitrogen content per dry mass (Nmass)",
                           "Leaf phosphorus content per dry mass (Pmass)",
                           "Leaf carbon content per dry mass", 
                           "Leaf nitrogen/phosphorus (N/P) ratio", 
                           "Leaf carbon/nitrogen (C/N) ratio",
                           "Leaf carbon/phosphorus (C/P) ratio") & UnitName != "mg/g"))
#seems like no ratio columns aren't in g/g...

#for summary of data coverage prior to pivoting:
View(rtry_explore(trydata_pruned, AccSpeciesName, DataName, sortBy = DataName))

#only numerical values
pivoted <- pivot_wider(trydata_pruned, id_cols = ObservationID,
                      names_from = DataName, values_from = c(StdValue))

pivoted <- pivoted%>% select(!c("Location Country", "Location Region",
                                "Location Continent"))

#location info
metadata <- pivot_wider(trydata_pruned, id_cols = ObservationID,
    names_from = DataName, values_from = OrigValueStr)

metadata <- metadata %>% select("ObservationID", "Location Country", "Location Region",
                                "Location Continent")

#join both
trait_data <- left_join(pivoted, metadata, by = "ObservationID")


#rename trait columns
trait_data <- rename(.data = trait_data,
       leaf_N = `Leaf nitrogen content per dry mass (Nmass)`,
       leaf_P = `Leaf phosphorus content per dry mass (Pmass)`,
       leaf_C = `Leaf carbon content per dry mass`,
       #NP_ratio = `Leaf nitrogen/phosphorus (N/P) ratio`, 
       #CN_ratio = `Leaf carbon/nitrogen (C/N) ratio`,
       #CP_ratio = `Leaf carbon/phosphorus (C/P) ratio`, 
       continent = `Location Continent`,
       region = `Location Region`,
       country = `Location Country`)

#remove blankspaces
trait_data[trait_data == ""] <- NA 

#rename continents properly
View(as.data.frame(unique(trait_data$continent)))

trait_data <- trait_data %>% 
  mutate(continent = recode(continent,
                            "Aus"= "Australia",
                            "E" = "Europe",
                            "NAm" = "North America",
                            "SAm" = "South America",
                            "Asi" = "Asia",
                            "Af" = "Africa",
                            "Pac" = "Pacific",
                            "Afr" = "Africa",
                            "Eur" = "Europe",
                            "Austrtalia" = "Australia"))


#calculate stoich ratios
#have to be careful when values are smaller than 1
#due to division

#i wonder how they calculated it if they're smaller than one
#edit: they were in g/g, so no values less than one most likely

#look at data coverage
coverage <- trait_data %>%
  group_by(continent, region, country) %>%
  summarise(
    n_total = n(),
    n_leaf_N = sum(!is.na(leaf_N)),
    n_leaf_P = sum(!is.na(leaf_P)),
    n_leaf_C = sum(!is.na(leaf_C)),
    #n_CN_ratio = sum(!is.na(CN_ratio)),
    #n_NP_ratio = sum(!is.na(NP_ratio)),
    #n_CP_ratio = sum(!is.na(CP_ratio)),
    .groups = "drop"
  )
#lots of data not in their respective continent
#basically no CP ratio data (edit, removed all ratio data)

#can fill in continent and country by latlong
location_data <- trait_data %>% select(ObservationID, Latitude, Longitude,
                                       continent, region, country)

library(sf)
library(rnaturalearth)
library(dplyr)

# 1. Load world data with country and continent info
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. Convert your data frame to sf, dropping rows with NA coordinates
location_data_clean <- location_data %>%
  filter(!is.na(Longitude), !is.na(Latitude))

points_sf <- st_as_sf(location_data_clean, coords = c("Longitude", "Latitude"), crs = 4326)

# 3. Join your points to country/continent polygons
points_with_geo <- st_join(points_sf, world[, c("name_long", "continent")], left = TRUE)

geo_info <- points_with_geo %>% 
  st_drop_geometry() %>% 
  select(ObservationID, continent.x, country, region, name_long, continent.y)

geo_info <- geo_info %>%
  mutate(
    continent = coalesce(continent.x, continent.y),
    country  = coalesce(country, name_long)
  ) %>%
  select(-continent.x, -continent.y, -name_long)
#if all three (continent, region, and country) NA, then no lat long associated
#so just NAs

inspect <- geo_info %>% filter(is.na(continent))

#assign continents to fill in (only 20)
country_to_continent <- tibble::tibble(
  country = c("Portugal", "Belize", "Italy", "Puerto Rico", "Turkey", "China", 
              "New Zealand", "USA", "Japan", "Australia", "Denmark", "Mexico", 
              "South Africa", "Brazil", "Spain", "France", "Canada", 
              "New Caledonia", "Solomon"),
  continent = c("Europe", "Central America", "Europe", "North America", "Asia", "Asia", 
                "Oceania", "North America", "Asia", "Oceania", "Europe", "Central America", 
                "Africa", "South America", "Europe", "Europe", "North America", 
                "Oceania", "Oceania"))

geo_info <- geo_info %>%
  left_join(country_to_continent %>% rename(continent_fill = continent), by = "country") %>%
  mutate(continent = coalesce(continent, continent_fill)) %>%
  select(-continent_fill)

inspect <- geo_info %>% filter(is.na(continent))

region_to_continent <- tibble::tibble(
  region = c("Zhanjiang city,Guangdong", "Weizhou island,Guangxi","Umiujaq, QC, Canada",
             "Deception Bay Quebec", "Lykteneset, Svalbard"),
  continent = c("China", "China", "Canada", "Canada", "Europe"))

geo_info <- geo_info %>%
  left_join(region_to_continent %>% rename(continent_fill = continent), by = "region") %>%
  mutate(continent = coalesce(continent, continent_fill)) %>%
  select(-continent_fill)

rm(age, country_to_continent, points_sf, inspect, location_data, location_data_clean,
   metadata, points_sd, points_with_geo, region_to_continent, world)

#merge geo info into trait data
trait_data <- trait_data %>%
  left_join(geo_info, by = "ObservationID") %>%
  mutate(
    country = coalesce(country.y, country.x),
    region = coalesce(region.y, region.x),
    continent = coalesce(continent.y, continent.x)
  ) %>%
  select(-country.x, -country.y,
         -region.x, -region.y,
         -continent.x, -continent.y)

#calculate ratios
#must ensure when denom is 0, resulting ratio is NA
trait_data <- trait_data %>%
  mutate(
    NP_ratio = ifelse(leaf_P == 0 | is.na(leaf_P), NA, leaf_N / leaf_P),
    CN_ratio = ifelse(leaf_N == 0 | is.na(leaf_N), NA, leaf_C / leaf_N),
    CP_ratio = ifelse(leaf_P == 0 | is.na(leaf_P), NA, leaf_C / leaf_P)
  )

#log them before plotting
trait_data <- trait_data %>%
  mutate(
    ln_NP_ratio = if_else(NP_ratio <= 0 | is.na(NP_ratio), NA_real_, log(NP_ratio)),
    ln_CN_ratio = if_else(CN_ratio <= 0 | is.na(CN_ratio), NA_real_, log(CN_ratio)),
    ln_CP_ratio = if_else(CP_ratio <= 0 | is.na(CP_ratio), NA_real_, log(CP_ratio))
  )

coverage <- trait_data %>%
  group_by(continent, region, country) %>%
  summarise(
    n_total = n(),
    n_leaf_N = sum(!is.na(leaf_N)),
    n_leaf_P = sum(!is.na(leaf_P)),
    n_leaf_C = sum(!is.na(leaf_C)),
    n_NP_ratio = sum(!is.na(NP_ratio)),
    n_CN_ratio = sum(!is.na(CN_ratio)),
    n_CP_ratio = sum(!is.na(CP_ratio)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = starts_with("n_"), names_to = "variable", values_to = "count")

#ensure Australia as country turned into continent
#then continent australia only associated with country australia

trait_data <- trait_data %>%
  mutate(
    country = case_when(
      country %in% c("Australia-ACT", "Australia-SA", "Australia-FNQ", 
                     "Australia-WA", "Australia-TAS", "Australia-NSW", 
                     "Austrtalia") ~ "Australia",
      TRUE ~ country
    )
  )

trait_data <- trait_data %>%
  mutate(
    continent = case_when(
      country == "Australia" ~ "Australia", #fix country to continent
      continent == "Australia" & country != "Australia" ~ "Oceania",
      #when Aus continent but country not Aus
      TRUE ~ continent
    )
  )


#----- plots!!!

#do a boxplot by continent
length(unique(trait_data$continent))
length(unique(trait_data$country))

#note that Oceania is anything that's not explicitely in Australia
ggplot(trait_data, aes(x = continent, y = leaf_C, color = continent)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01) +
  coord_cartesian(ylim = c(0, 1100)) + 
  theme_classic()

#data coverage
ggplot(coverage, aes(x = variable, y = continent, size = count, fill = continent)) +
  geom_point(shape = 21, color = "black", alpha = 0.7) +
  scale_size_area(max_size = 15) +
  theme_minimal() +
  labs(title = "Data Coverage by Continent",
       x = "Variable",
       y = "Continent",
       size = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#map
library(maps)

world_map <- map_data("world")
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") + theme_minimal() +
  geom_point(data = pivoted, aes(x = Longitude, y = Latitude), color = "darkgreen", size = 0.2) +
  labs(title = "trait obs", x = "Longitude", y = "Latitude")