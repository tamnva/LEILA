
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

# setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")

# Initialize some variales
hydro_indicator <- NULL
near_nat_states <- NULL

# Time series data
timeseries_camels_combine_file <- "data/CAMELS_DE_hydromet_timeseries_combine.csv"

# Read catchment attributes, stations, catchments
attributes  <- read_csv("data/attributes.csv", show_col_types = FALSE) 
stations    <- st_read("data/CAMELS_DE_gauging_stations.shp", quiet = TRUE)
catchments  <- st_read("data/CAMELS_DE_catchments.shp", quiet = TRUE)

# Schutzgebiete, Nitratbelastete Gebiete, Hydrogeologische Einheiten
schutzgetbiet           <- st_transform(st_read("data/schutzgebiet", 
                                                quiet = TRUE), 4326)
nitratbelastete_gebiete <- st_read("data/Nitratbelastete_Gebiete.shp", 
                                   quiet = TRUE)
huek                    <- rast("data/huek.tif")

# Read groundwater wells
gw_wells <- read.csv("data/gw_meta_monthly_geo_qc.txt", sep = ";", 
                     header = TRUE) %>% as_tibble()
gw_wells <- st_as_sf(gw_wells, coords = c("x_EPSG25832", "y_EPSG25832"), 
                     crs = 25832, remove = FALSE)
gw_wells <- st_transform(gw_wells, 4326)


# Add nitrate polluted and protected areas into the attributes. Run again only 
# if you replace the nitratbelastete_gebiete an schutzgetbiet maps with new maps
if (FALSE){
  
  source("R/overlappingArea.R")
  
  attributes <- attributes %>% 
    left_join(overlappingArea(catchments, nitratbelastete_gebiete) %>%
                rename(nitrate_polluted_area_fraction = percent_cover), 
              by = "gauge_id")
  
  # Add protected areas into the attributes
  attributes <- attributes %>% 
    left_join(overlappingArea(catchments, schutzgetbiet) %>%
                rename(protected_area_fraction = percent_cover), 
              by = "gauge_id")
  
  data.table::fwrite(attributes, "data/attributes.csv")
}

message(" Done reading all data")
