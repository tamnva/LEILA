#' Multi-linear regression models, try with different combination of indepdent 
#' variables and take the best model based on the lowest AIC 
#'
#' @param data dataframe; dataframe of dependent and independent variables
#' 
#' @return Best fitted model
#'
#' @examples
#'
#' @export

multiLinearReg <- function(data, dependent_var, independent_var){
  
  form <- as.formula(
    paste(dependent_var, "~", paste(independent_var, collapse = "+"))
  )
  
  full_model <- lm(form, data = data)
  step_model <- step(full_model, direction = "both", trace = TRUE)
  
  return(step_model)
}
