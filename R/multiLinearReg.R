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
  
  step_model <- list()
  plt <- list()
  
  for (var in dependent_var){
    
    # find model
    form <- as.formula(paste(var, "~", paste(independent_var, collapse = "+")))
    full_model <- lm(form, data = data)
    step_model[[var]] <- step(full_model, direction = "both", trace = TRUE)
    
    # plot results 
    fitted <- step_model[[var]]$fitted.values
    actual <- data[[var]]
    
    plt[[var]] <- ggplotly(
      ggplot() + 
        geom_point(aes(x = fitted, y = actual), alpha = 0.4, size = 1, 
                   color = "#1E88E5")+
        labs(x = paste0("Fitted values (", var, ")"), y = "Actual values") +
        theme_bw() +
        theme(axis.title = element_text(size = 10),
              text = element_text(family = "Arial"))
    )
  }
  
  step_model$plt <- plt
  
  return(step_model)
}

