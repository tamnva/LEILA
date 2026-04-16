
#------------------------------------------------------------------------------#
#                      Load all require packages                               #
#                          Read all base data                                  #
# The sources of these data are described in the README.txt file (same folder) #
#------------------------------------------------------------------------------#

library(leaflet)
library(spsComps)
library(plotly)
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
library(plotly)
library(randomForest)

# setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")

# Time series data
timeseries_camels <- "data/CAMELS_DE_hydromet_timeseries_combine.csv"

# Read catchment attributes and hydrological indicators, stations, catchments
att_hydro  <- read_csv("data/attributes_and_hydrological_indicator.csv") 
stations    <- st_read("data/CAMELS_DE_gauging_stations.shp")
catchments  <- st_read("data/CAMELS_DE_catchments.shp")

# Schutzgebiete, Nitratbelastete Gebiete, Hydrogeologische Einheiten
schutzgetbiet           <- st_transform(st_read("data/schutzgebiet"), 4326)
nitratbelastete_gebiete <- st_read("data/Nitratbelastete_Gebiete.shp")
huek                    <- rast("data/huek.tif")
population_density      <- rast("data/population_density.tif")

# Read groundwater wells
selected_wells <- read.csv("data/selected_well_per_catchment_unique.csv")
gw_wells <- read.csv("data/gw_meta_monthly_geo_qc.txt", 
                     sep = ";", header = TRUE) %>% as_tibble() %>%
  filter(UFZ.ID %in% selected_wells$well_id)

gw_wells <- st_as_sf(gw_wells, coords = c("x_EPSG25832", "y_EPSG25832"), 
                     crs = 25832, remove = FALSE)
gw_wells <- st_transform(gw_wells, 4326)

# Read abwassermenge
waste_water_discharge <- st_read("data/abwassermenge.geojson", quiet = TRUE)

message(" Done reading all data")
