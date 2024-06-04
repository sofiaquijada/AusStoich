raw_leaf_data <- read.csv("Inputs/austraits_leaf_stoichiometry_MASTER.csv",
                          header = TRUE)
raw_env_data <- read.csv("Inputs/austoich_gridded_env_data.csv",
                         header = TRUE)
#not all latlongs are present
#raw env data: lat 4 dp, long 3dp
#raw leaf data: different dp for lat long
raw_extra_env_data <- read.csv("Inputs/austoich_extra_env_data.csv",
                               header = TRUE)

#match latlongs
env_data <- merge(raw_env_data, raw_extra_env_data, by = c("lat", "lon"))
#works, but goes from 356 to 89 rows

#function to round decimal places + standardize
decimal_places <- 3
round_lat_lon <- function(df, decimal_places) {
  df %>%
    mutate(
      lat = round(lat, decimal_places),
      lon = round(lon, decimal_places)
    )
}
rounded_raw_env_data <- round_lat_lon(raw_env_data, decimal_places) #356
rounded_raw_extra_env_data <- round_lat_lon(raw_extra_env_data, decimal_places) #356

#rounded_raw_env_data$latlongr <- paste(rounded_raw_env_data$lat,
                                      # rounded_raw_env_data$lon,sep = "")
#rounded_raw_extra_env_data$latlongr <- paste(rounded_raw_extra_env_data$lat,
                                          #   rounded_raw_extra_env_data$lon, sep = "")

rounded_env_data <- merge(rounded_raw_env_data,
                          rounded_raw_extra_env_data, by = c("lat","lon")) #355 

rounded_raw_extra_env_data <- rounded_raw_extra_env_data[order(-rounded_raw_extra_env_data$lat) ]

rounded_raw_env_data <- rounded_raw_env_data[order(-rounded_raw_env_data[lat]), ]

#355 entries, need 356. find missing entry. probably both lat and lon rounded dif
rounded_env_data <- merge(rounded_raw_env_data,
                          rounded_raw_extra_env_data, by = c("latlongr"))

rounded_env_data[!(rounded_env_data$latlongr %in% rounded_raw_env_data$latlongr)]
rounded_env_data[!(rounded_env_data$latlongr %in% rounded_raw_extra_env_data$latlongr)]

rounded_raw_env_data[!(rounded_raw_env_data$latlongr %in% rounded_raw_extra_env_data$latlongr)]

rounded_env_data <- merge(rounded_raw_env_data,
                          rounded_raw_extra_env_data, by = c("latlongr"))

#STILL 0 HUH???????

rounded_raw_env_data[!(rounded_raw_env_data$lat %in% rounded_raw_extra_env_data$lat
                       & rounded_raw_env_data$lon%in% rounded_raw_extra_env_data$lon), ]
#in df1 but not in df2

rounded_raw_extra_env_data[!(rounded_raw_extra_env_data$lat %in% rounded_raw_env_data$lat
                       & rounded_raw_extra_env_data$lon%in% rounded_raw_env_data$lon), ]

rounded_env_data[!(rounded_env_data$lat %in% rounded_raw_env_data$lat
                             & rounded_env_data$lon%in% rounded_raw_env_data$lon), ]
rounded_env_data[!(rounded_env_data$lat %in% rounded_raw_extra_env_data$lat
                   & rounded_env_data$lon%in% rounded_raw_extra_env_data$lon), ]

rounded_env_data[!(rounded_env_data$lat %in% rounded_raw_env_data$lat)]#0
rounded_env_data[!(rounded_env_data$lat %in% rounded_raw_extra_env_data$lat)]#0
rounded_env_data[!(rounded_env_data$lon %in% rounded_raw_env_data$lon)]#0
rounded_env_data[!(rounded_env_data$lon %in% rounded_raw_extra_env_data$lon)]#0





