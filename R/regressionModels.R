#' Multi-linear regression models (reg_models), try with different combination of indepdent 
#' variables and take the best model based on the lowest AIC 
#'
#' @param data dataframe; dataframe of dependent and independent variables
#' 
#' @return Best fitted model
#'
#' @examples
#'
#' @export

reg_models <- function(data,          # data for training and testing
                dependent_var,        # names of dependent variables
                independent_var,      # name independent variables
                model_name="Multiple linear regression",    # model name 
                n_train_samples = 80  # 80% used for training, 20% testing
                ){
  
  models <- list()
  models$plt <- list()
  models$simulated <- list()
  models$rmse_train <- list()
  models$rmse_test <- list()
  
  # Split data to training and testing
  index <- sample(c(1:nrow(data)))
  istart <- as.integer(nrow(data) * n_train_samples/100)
  data_train <- data[1:istart,]
  data_test <- data[-c(1:istart),]
  
  #----------------------------------------------------------------------------#
  # Construct different models
  for (var in dependent_var){
    
    # Regression formular
    form <- as.formula(paste(var, "~", paste(independent_var, collapse = "+")))
    
    # Multi-linear regression models
    if (model_name == "Multiple linear regression"){  
      full_model <- lm(form, data = data_train)
      models[[var]] <- step(full_model, direction = "both", trace = TRUE)
      models$simulated[[var]] <- models[[var]]$fitted.values
      
      models$rmse_test[[var]] <- sqrt(mean(
        data_test[[var]] - predict(models[[var]], data_test))^2)
      
    # Random forest
    } else if(model_name == "Random forest"){
      
      models[[var]] <- randomForest(form, data = data_train, importance = TRUE)
      
      models$rmse_test[[var]] <- sqrt(mean(
        data_test[[var]] - predict(models[[var]], data_test))^2)
      
    } else {
  
    }
    
  }
  
  #----------------------------------------------------------------------------#
  # Plot results
  for (var in dependent_var){
    target_test <- data_test[[var]]
    predict_test <- predict(models[[var]], data_test)
  
    target_train <- data_train[[var]]
    predict_train <- predict(models[[var]], data_train)
    

    models$plt[[var]] <- ggplotly(
      ggplot() + 
        geom_point(aes(x =  target_test, y = predict_test), 
                   color = "#1E88E5", alpha = 0.7, size = 1) +
        geom_point(aes(x =  target_train, y = predict_train), 
                   color = "#D81B60", alpha = 0.7, size = 1) +
        scale_color_discrete(name = "Model performance") +
        labs(x = "Target", y = "Predicted", title = var) +
        theme(axis.title = element_text(size = 10),
              text = element_text(family = "Arial")) +
        theme_bw()
    )
  }
  
  return(models)

}

