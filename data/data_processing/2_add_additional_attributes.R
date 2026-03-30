#-----------------------------------------------------------------------------#
# Rcode for adding the nitrate_polluted_area_fraction                         #
#                       protected_area_fraction                               #
#                       popdens
#                       average waste water discharge
#  to the attribute tables                                                    #
#-----------------------------------------------------------------------------#
setwd("C:/Users/nguyenta/Documents/LEILA/working_code_documentation/code/leila_visualization")

source("R/overlappingArea.R")
library(ffm)
library(exactextractr)
library(terra)
library(sf)
library(dplyr)

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
population_density      <- rast("data/population_density.tif")

# Read groundwater wells
gw_wells <- read.csv("data/gw_meta_monthly_geo_qc.txt", sep = ";", 
                     header = TRUE) %>% as_tibble()
gw_wells <- st_as_sf(gw_wells, coords = c("x_EPSG25832", "y_EPSG25832"), 
                     crs = 25832, remove = FALSE)
gw_wells <- st_transform(gw_wells, 4326)


# Read abwassermenge
waste_water_discharge <- st_read("data/abwassermenge.geojson", quiet = TRUE)


# Add nitrate pollutated area to the attributes table
attributes <- attributes %>% 
  left_join(overlappingArea(catchments, nitratbelastete_gebiete) %>%
              rename(nitrate_polluted_area_fraction = percent_cover), 
            by = "gauge_id")

# Add protected areas into the attributes
attributes <- attributes %>% 
  left_join(overlappingArea(catchments, schutzgetbiet) %>%
              rename(protected_area_fraction = percent_cover), 
            by = "gauge_id")

# Add population density map to the attributes
munics <- bkg_admin(level = "gem",key_date = "1231") %>% 
  mutate(popdens = ewz/kfl)
munics <- st_transform(munics, crs(catchments))
r <- rast(extent = ext(munics), resolution = 0.005, crs = crs(munics))
popdens <- rasterize(munics, r, field = "popdens", "mean", na.rm = TRUE)
catchments$popdens <- exact_extract(popdens, catchments, 'mean')

attributes <- attributes %>% 
  left_join(as.data.frame(catchments, geom = FALSE) %>% 
              select(c("popdens", "gauge_id")), by = "gauge_id")

catchments <- catchments %>% select(!c(popdens))

# Now add sum of waste water discharge to the attributes table
sum_discharge <- c()
for (icatchment in 1:nrow(catchments)){
  points_in_poly <- st_crop(waste_water_discharge, catchments[icatchment,])
  sum_discharge <- c(sum_discharge, sum(points_in_poly$durchschnitt_abwassermenge))
}
abwasser <- tibble(gauge_id = catchments$gauge_id,
                   wastewater_discharge_m3_year = sum_discharge)

attributes <- attributes %>% left_join(abwasser, by = "gauge_id")

data.table::fwrite(attributes, "data/attributes.csv")
writeRaster(popdens, "data/population_density.tif")