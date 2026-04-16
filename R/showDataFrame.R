
#' Display dataframe (or subset of its) in shiny link with map
#'
#' @param attributes dataframe; catchment attributes
#' @param session R session
#' @param select_gauge_id vector, selected gauges
#' 
#' @return data frame display in R with option to click to the selected stations
#' and it will go to the interactive map
#'
#' @examples
#'
#' @export
#' 
showDataFrame <- function(att_hydro, session, 
                          outputId, select_gauge_id=NULL){
  
  if (!is.null(select_gauge_id)) {
    dataFrame <- att_hydro %>% 
      dplyr::filter(gauge_id %in% select_gauge_id)
  } else {
    dataFrame <- att_hydro
  }
  
  dataFrame <- dataFrame %>%
    dplyr::mutate(Show = paste('<a class="go-map" href="" data-lat="', 
                        lat, '" data-long="', 
                        long, '" data-zip="', 
                        gauge_id, '"><i class="fa fa-crosshairs"></i></a>', 
                        sep="")) %>% 
    dplyr::select(last_col(), everything())
  
  action <- DT::dataTableAjax(session, dataFrame, 
                              outputId = outputId)
  
  DT::datatable(dataFrame, options = list(ajax = list(url = action)), 
                escape = FALSE)
}