
#' Show notification
#'
#' @param regression_method character; regression method
#' 
#' @return pop up message on shiny interface
#'
#' @examples
#'
#' @export

show_notification <- function(regression_method){
  
  if (regression_method == "Multiple Linear Regression"){
    showNotification(
      "Multi-linear regression option allows to predict only 1 dependent variables",
      type = "message", 
      duration = 3
    )
  } else {
    
  }

}