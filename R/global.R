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



# setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")
#------------------------------------------------------------------------------#
#                         Read database                                        #
#------------------------------------------------------------------------------#
timeseries_camels_combine_file = "data/CAMELS_DE_hydromet_timeseries_combine.csv"

# Read catchment attributes
attributes <- read.csv("data/attributes.csv", header = TRUE, sep = ",") %>%
  tibble::as_tibble() %>% rename(lat = gauge_lat, long = gauge_lon)

# Read shape files of station and catchments
stations <- sf::st_transform(st_read("data/CAMELS_DE_gauging_stations.shp", 
                                     quiet = TRUE), 4326) %>%
  dplyr::mutate(long = st_coordinates(geometry)[1],
                lat = st_coordinates(geometry)[2])

catchments <- sf::st_transform(st_read("data/CAMELS_DE_catchments.shp", 
                                       quiet = TRUE), 4326) 

hydro_indicator <- NULL
selected_catchment <- NULL

#Read Hydrogeologische Einheiten (from huek250 map from BGR)
huek <- rast("data/huek.tif")

#------------------------------------------------------------------------------#
#                     Ultility functions for server                            #
#------------------------------------------------------------------------------#