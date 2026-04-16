
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

#setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")

# Time series data
timeseries_camels <- "data/CAMELS_DE_hydromet_timeseries_combine.csv"

# Read catchment attributes and hydrological indicators
att_hydro  <- read_csv("data/attributes_and_hydrological_indicator.csv", 
                       show_col_types = FALSE)

# Read station shape file
stations <- st_read("data/CAMELS_DE_gauging_stations.shp", 
                       quiet = TRUE)

# Read catchment shape file
catchments  <- st_read("data/CAMELS_DE_catchments.shp",
                       quiet = TRUE)

# Read nature projection area shpe file
schutzgetbiet <- st_transform(st_read("data/schutzgebiet", quiet = TRUE), 4326)

# Read nitrate polluted area shape file
nitratbelastete_gebiete <- st_read("data/Nitratbelastete_Gebiete.shp", 
                                   quiet = TRUE)

# Read hydrogeological map
huek <- rast("data/huek.tif")

# Read population density map
population_density <- rast("data/population_density.tif")

# Read groundwater wells
selected_wells <- read_csv("data/selected_well_per_catchment_unique.csv", 
                           show_col_types = FALSE)

gw_wells <- read.csv("data/gw_meta_monthly_geo_qc.txt", 
                     sep = ";", header = TRUE) %>% 
  as_tibble() %>% filter(UFZ.ID %in% selected_wells$well_id)

gw_wells <- st_as_sf(gw_wells, coords = c("x_EPSG25832", "y_EPSG25832"), 
                     crs = 25832, remove = FALSE)

gw_wells <- st_transform(gw_wells, 4326)

# Read waste water discharge
waste_water_discharge <- st_read("data/abwassermenge.geojson", quiet = TRUE)

message(" Done reading all data")
