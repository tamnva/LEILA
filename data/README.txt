File_name                                    Source
CAMELS_DE_catchments.shp                     CAMELS-DE https://camels-de.org/
attributes_and_hydrological_indicator.csv    Combine from all attributes files from https://camels-de.org/
huek.tif                                     Main hydrogeological unit, aggregated from this data https://www.bgr.bund.de/DE/Themen/Grundwasser/Projekte/Flaechen-Rauminformationen/Huek250/huek250.html
schutzgebiet                                 Protected area from schutzgebiet https://www.geoportal.de/Download/bec888f9-ba0c-42dc-846e-177b8265dafa
Nitratbelastete_Gebiete.shp                  Nitrate polluted area, convert to shp file from json UBA https://metadaten.uba.de/smartfinder-client/?lang=de#/datasets/iso/07e1b760-397c-403c-8dc0-441c25b7195e using this R code 
population_density.tif                       Taken from the Germany’s Federal Agency for Cartography and Geodesy using the ffm package in R with the following command R script data_processsing/population_density.R
abwassermenge.geojson                        Taken from https://kommunales-abwasser.de/modal_downloads2022.html, data from 2008 to 2022 were combined all together and get the average value and converted to geojson fiel using the R script in data_processsing/abwassermenge.R

# Catchment delineation for salto
Y:\Gruppen\gw-data\Ralf_Transfer\D_SALTO\SALTO_V23\SALTO_SETUPSALTO_model_SETUP_subbasin_main.R  
