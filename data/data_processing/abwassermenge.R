
library(readr)
library(dplyr)
library(sf)
library(leaflet)

# Set working directory
setwd("C:/Users/nguyenta/Downloads")
files <- list.files(pattern = ".csv", full.names = TRUE)
data <- list()
lnames <- as.character(substr(basename(files), 1, 4))

# Read raw data
for (fname in files){
  lname <- as.character(substr(basename(fname), 1, 4))
  data[[lname]] <- read.csv(csvfiles[1], fileEncoding = "latin1", 
                   header = TRUE, sep = ";") %>% as_tibble()
}

# Merge data together
merge_data <- data[[lnames[1]]] %>% select(!abwassermenge)
merge_data[[paste0("abwassermenge_", lnames[1])]] <- data[[lnames[1]]]$abwassermenge
merge_data$merge_lat <- as.character(round(merge_data$geo_wgs84_lat, 10))
for (lname in lnames[-c(1)]){
  temp <- data[[lname]]
  same_lat <- setequal(round(data[[lnames[1]]]$geo_wgs84_lat, 10), 
                       round(temp$geo_wgs84_lat, 10))
  same_long <- setequal(round(data[[lnames[1]]]$geo_wgs84_long, 10), 
                        round(temp$geo_wgs84_long, 10))
  if (same_lat & same_long){
    temp$merge_lat <-  as.character(round(temp$geo_wgs84_lat, 10))
    temp <- temp %>% select(c(merge_lat, abwassermenge))
    temp <- temp %>% rename(!!paste0("abwassermenge_", lname) := "abwassermenge")
    merge_data <- merge_data %>% left_join(temp,by = "merge_lat") 
  } else {
    message("Cannot merge data due to differences in lat lon")
  }
}

merge_data <- merge_data %>%
  mutate(durchschnitt_abwassermenge = 
           rowMeans(across(starts_with("abwassermenge_20")), na.rm = TRUE))


# Convert to GIS
merge_data <- st_as_sf(merge_data, 
                       coords = c("geo_wgs84_long", "geo_wgs84_lat"), crs = 4326)

merge_data <- merge_data %>% 
  select(!merge_lat) %>% 
  select(c("kläranlagenname", 
           "abwassermenge_einheit", 
           "durchschnitt_abwassermenge",
           "abwassermenge_2008",  
           "abwassermenge_2010",
           "abwassermenge_2012",
           "abwassermenge_2014",
           "abwassermenge_2016",
           "abwassermenge_2018",
           "abwassermenge_2020",
           "abwassermenge_2022" ))

# Visualize 
color_values <- merge_data$durchschnitt_abwassermenge
breaks <- quantile(color_values, probs = seq(0, 1, length.out = 10), 
                   na.rm = TRUE)
pal <- colorBin(palette = "viridis", domain = color_values, bin = breaks)
leaflet(merge_data) %>%
  addTiles() %>%
  addCircleMarkers(radius = 2,
                   group = "Grundwassermessstelle",
                   fillColor =  pal(color_values),
                   fillOpacity = 0.7,
                   stroke = FALSE) %>%
  addLegend(
    pal = pal,
    values = color_values,
    position = 'topleft',
    title = "abwassermenge (m3/s)",
    opacity = 1,
    labFormat = labelFormat(digits = 0)
  ) 

st_write(st_transform(merge_data, 4326), "data/abwassermenge.geojson",
         delete_layer = TRUE)
