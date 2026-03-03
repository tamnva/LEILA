File_name                             Source
CAMELS_DE_catchments.shp              CAMELS-DE https://camels-de.org/
attributes.csv                        Combine from all attributes files from https://camels-de.org/
huek.tif                              Main hydrogeological unit, aggregated from this data https://www.bgr.bund.de/DE/Themen/Grundwasser/Projekte/Flaechen-Rauminformationen/Huek250/huek250.html
schutzgebiet                          Protected area from schutzgebiet https://www.geoportal.de/Download/bec888f9-ba0c-42dc-846e-177b8265dafa
Nitratbelastete_Gebiete.shp           Nitrate polluted area, convert to shp file from json UBA https://metadaten.uba.de/smartfinder-client/?lang=de#/datasets/iso/07e1b760-397c-403c-8dc0-441c25b7195e
population_density.tif                Taken from the Germany’s Federal Agency for Cartography and Geodesy using the ffm package in R with the following command (R code below)

#-----------------------------------------------------------------------------#
# Rcode for adding the nitrate_polluted_area_fraction                         #
#                       protected_area_fraction                               #
#                       popdens                                               #
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

# Water use also abwasserkarl
Y:\Gruppen\gw-data\Ralf_Transfer\D_SALTO\SALTO_V23\SALTO_SETUP\SALTO_CAT_WU_WWTP.R
https://kommunales-abwasser.de/modal_downloads2022.html

# Catchment delineation for salto
Y:\Gruppen\gw-data\Ralf_Transfer\D_SALTO\SALTO_V23\SALTO_SETUPSALTO_model_SETUP_subbasin_main.R  
