#' Show popup message when clicking the gauge or basin shape file
#'
#' @param gauge_id Numeric or character; indicating gauged number of ID
#' @param lat Numeric; latitude of the mouse/pointer
#' @param lng Numeric; longitude of the mouse/pointer
#' 
#' @return A popup information about the gauge or catchment
#'
#' @examples
#'
#' @export

# Show a popup at the given location
showPopup <- function(gauge_id) {
  
  selectedCat <- att_hydro[att_hydro$gauge_id == gauge_id[1],]

  content <- as.character(tagList(
    tags$h5("Gauge ID:", selectedCat$gauge_id),
    sprintf("Catchment area: %s %s", round(selectedCat$area, 0), " km²"), 
    tags$br(),
    sprintf("Mean elevation: %s %s", round(selectedCat$elev_mean, 0), " m"), 
    tags$br(),
    sprintf("Forest and semi-nattural area: %s %s", 
            round(selectedCat$forests_and_seminatural_areas_perc, 0), " %")
  ))
}
