#' Adaptative linear model
#'
#' Adjust sequentially a linear model based on feature combinations and AIC penalty method
#' The model is trained on a rolling-window and updated including past residuals observed on test set
#'
#' @param data_train the training set
#' @param data_test the test set
#' @param update_frequency the size of the time window in days before a model update
#' @param rolling_window the size of the time window in days used to train the models
#'
#' @return a vector containing the forecasts obtained with the adjusted models
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export



adaptative_linear_model <- function(data_train, data_test, update_frequency = 50, rolling_window = 500){
  train_size = nrow(data_train)
  test_size = nrow(data_test)
  periods <- floor(test_size/update_frequency)
  size_last_period <- test_size%%update_frequency
  
  initial_fit <- lm(Load ~(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time)
                    *(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time),
                    data= data_train[-(1:(train_size-rolling_window)),])
  model_step <- MASS::stepAIC(initial_fit, direction="both", trace = FALSE)
  forecast = c(predict(model_step, newdata = data_test[1:update_frequency,]))
  
  # reformatting test set before joining it with train set for a continuous rolling window
  data_test_reformat = cbind(data_test$Date,
                             c(data_test$Load.1[2:test_size],data_test$Load.1[test_size]),
                             subset(data_test, select = -c(Id,Date)))
  colnames(data_test_reformat)[1:2] <- c("Date","Load")
  aggregated_data = rbind(data_train[-(1:(train_size-rolling_window)),],data_test_reformat)
  
  for(i in 1:(periods-1)){
    data_period = aggregated_data[(i*update_frequency):(rolling_window+i*update_frequency),]
    period_fit <- lm(Load ~(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time)
                     *(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time),
                     data= data_period)
    model_period_step <- MASS::stepAIC(period_fit, direction="both", trace = FALSE)
    forecast = c(forecast,predict(model_period_step, newdata = data_test[((i*update_frequency)+1):((i+1)*update_frequency),]))
  }
  for(i in periods){
    data_period = aggregated_data[(i*update_frequency):(rolling_window+i*update_frequency),]
    period_fit <- lm(Load ~(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time)
                     *(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time),
                     data= data_period)
    model_period_step <- MASS::stepAIC(period_fit, direction="both", trace = FALSE)
    forecast = c(forecast,predict(model_period_step, newdata = data_test[((i*update_frequency)+1):test_size,]))
  }
  return(forecast)
}