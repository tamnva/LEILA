

showGauge <- function(stations_shape, select_gauge_id){
  
  # Update map
  leafletProxy("map") %>%
    clearGroup("Alle Einzugsgebiete") %>%
    addCircleMarkers(data = stations_shape %>% 
                       dplyr::filter(
                         gauge_id %in% select_gauge_id),
                     radius = 3,
                     group = "Alle Einzugsgebiete",
                     fillColor = "#FFC107",
                     fillOpacity = 0.8,
                     stroke = FALSE,
                     layerId = ~ gauge_id
    ) %>%
    clearControls()
}