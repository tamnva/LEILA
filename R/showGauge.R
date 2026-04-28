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
      clearGroup("Streamgauge") %>%
      addCircleMarkers(data = stations_shape,
                       radius = 3,
                       group = "Streamgauge",
                       fillColor = "#FFC107",
                       fillOpacity = 0.8,
                       stroke = FALSE,
                       layerId = ~ gauge_id
      ) %>%
      clearControls()

  } else {

    color_values <- as.vector(stations_shape[[colorby]])
    breaks <-  c(-Inf, -100, -75, -50, -25, 25, 50, 75, 100, Inf)
    pal <- colorBin(palette = "PiYG", domain = color_values, bin = breaks)

    leafletProxy("map") %>%
      clearGroup("Streamgauge") %>%
      addCircleMarkers(data = stations_shape,
                       radius = 3,
                       group = "Streamgauge",
                       fillColor = pal(color_values),
                       stroke = FALSE,
                       fillOpacity = 0.8,
                       layerId = ~ gauge_id
      ) %>%
      clearControls() %>%
      addLegend(
        pal = pal,
        values = color_values,
        position = 'topleft',
        title = "Distance to near nat. (%)",
        opacity = 1,
        labFormat = labelFormat(digits = 0)
      )
  }
}
