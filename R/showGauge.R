#' Show map of selected gauges
#'
#' @param stations_shape polygon object; GIS map of gauges
#' @param select_gauge_id vector; selected station ids
#' 
#' @return leaflet map of selected gauging stations
#'
#' @export

showGauge <- function(stations_shape, 
                      select_gauge_id, 
                      colorby = NA){
  
  stations_shape <- stations_shape %>% 
    dplyr::filter(gauge_id %in% select_gauge_id)
  
  # Update map
  if (is.na(colorby)){
    leafletProxy("map") %>%
      clearGroup("Abflussmessstelle") %>%
      addCircleMarkers(data = stations_shape,
                       radius = 3,
                       group = "Abflussmessstelle",
                       fillColor = "#FFC107",
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       layerId = ~ gauge_id
      ) %>%
      clearControls()
    
  } else {
    
    color_values <- stations_shape[[colorby]] 
    pal <- colorNumeric(palette = "viridis", 
                        domain = color_values)
    
    leafletProxy("map") %>%
      clearGroup("Abflussmessstelle") %>%
      addCircleMarkers(data = stations_shape,
                       radius = 3,
                       group = "Abflussmessstelle",
                       fillColor = pal(color_values),
                       stroke = FALSE,
                       fillOpacity = 1,
                       layerId = ~ gauge_id
      ) %>%
      clearControls() %>%
      addLegend(
        pal = pal,
        values = color_values,
        position = 'topleft',
        title = "Distance to near nat. (%)",
        opacity = 1
      ) 
  }
}
