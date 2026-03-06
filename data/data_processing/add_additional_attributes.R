#-----------------------------------------------------------------------------#
# Rcode for adding the nitrate_polluted_area_fraction                         #
#                       protected_area_fraction                               #
#                       popdens
#                       average waste water discharge
#  to the attribute tables                                                    #
#-----------------------------------------------------------------------------#
                       
source("R/overlappingArea.R")
library(ffm)
library(exactextractr)
library(terra)

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

data.table::fwrite(attributes, "data/attributes.csv")
writeRaster(popdens, "data/population_density.tif")