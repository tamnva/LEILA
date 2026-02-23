library(leaflet)
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(sf)
library(terra)
library(tibble)
library(DT)
library(spsComps)
library(ggplot2)
library(patchwork)
library(trend)



# setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")
#------------------------------------------------------------------------------#
#                         Read database                                        #
#------------------------------------------------------------------------------#
timeseries_camels_combine_file = "data/CAMELS_DE_hydromet_timeseries_combine.csv"

# Read catchment attributes
attributes <- read.csv("data/attributes.csv", header = TRUE, sep = ",") %>%
  as_tibble() %>% 
  rename(lat = gauge_lat, long = gauge_lon)

# Read shape files of station and catchments
stations <<- st_transform(st_read("data/CAMELS_DE_gauging_stations.shp", 
                                     quiet = TRUE), 4326) %>%
  mutate(long = st_coordinates(geometry)[1],
                lat = st_coordinates(geometry)[2])

catchments <<- st_transform(st_read("data/CAMELS_DE_catchments.shp", 
                                       quiet = TRUE), 4326) 

# Schutzgebiete: https://www.geoportal.de/Download/bec888f9-ba0c-42dc-846e-177b8265dafa
schutzgetbiet <- st_transform(st_read("data/schutzgebiet"), 4326)

# Nitratbelastete Gebiete https://metadaten.uba.de/smartfinder-client/?lang=de#/datasets/iso/07e1b760-397c-403c-8dc0-441c25b7195e
nitratbelastete_gebiete <- st_transform(
  st_read("data/Nitratbelastete_Gebiete.geojson", quiet = TRUE), 4326) 

hydro_indicator <<- NULL
selected_catchment <<- NULL

#Read Hydrogeologische Einheiten (from huek250 map from BGR)
huek <- rast("data/huek.tif")
