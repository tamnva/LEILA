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
# Initialize some variales
hydro_indicator <<- NULL
near_nat_states <<- NULL

# Time series data
timeseries_camels_combine_file = "data/CAMELS_DE_hydromet_timeseries_combine.csv"

# Read catchment attributes, stations, catchments
attributes <<- read_csv("data/attributes.csv", show_col_types = FALSE) 
stations <<- st_read("data/CAMELS_DE_gauging_stations.shp", quiet = TRUE)
catchments <<- st_read("data/CAMELS_DE_catchments.shp", quiet = TRUE)

# Schutzgebiete, Nitratbelastete Gebiete, Hydrogeologische Einheiten
schutzgetbiet <<- st_transform(st_read("data/schutzgebiet"), 4326)
nitratbelastete_gebiete <<- st_read("data/Nitratbelastete_Gebiete.shp", quiet = TRUE)
huek <<- rast("data/huek.tif")

# Add the nitrate polluted and protected areas into the attributes
# Run this when you replace the new nitratbelastete_gebiete and schutzgetbiet maps
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
