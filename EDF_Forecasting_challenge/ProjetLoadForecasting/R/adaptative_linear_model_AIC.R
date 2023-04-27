#' Adaptative Linear model with AIC penalty and forgetting factor
#'
#' Refit linear models through time based on a rolling window of last observations
#' and penalizing with sigmoid weights the oldest training examples
#'
#'
#' @param data_train the train set
#' @param data_test the test set
#' @param update_frequency the number of days before refitting a linear model
#' @param rolling_window the number of days kept in memory for model training
#'
#' @return the forecast obtained
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


adaptative_linear_model_AIC <- function(data_train, data_test, update_frequency = 50, rolling_window = 1500){
  train_size = nrow(data_train)
  test_size = nrow(data_test)
  periods <- floor(test_size/update_frequency)
  size_last_period <- test_size%%update_frequency

  weights = sigmoid(seq(-6,3,length.out = rolling_window))+1

  initial_fit <- lm(Load ~(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time)
                    *(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time),
                    data= data_train[-(1:(train_size-rolling_window)),], weights = weights)
  model_step <- MASS::stepAIC(initial_fit, direction="both", trace = FALSE)
  forecast = c(predict(model_step, newdata = data_test[1:update_frequency,]))

  data_test_reformat = cbind(data_test$Date,
                             c(data_test$Load.1[2:test_size],data_test$Load.1[test_size]),
                             subset(data_test, select = -c(Id,Date)))
  colnames(data_test_reformat)[1:2] <- c("Date","Load")
  aggregated_data = rbind(data_train[-(1:(train_size-rolling_window)),],data_test_reformat)

  for(i in 1:(periods-1)){
    data_period = aggregated_data[(i*update_frequency):(rolling_window+i*update_frequency),]
    period_fit <- lm(Load ~(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time)
                     *(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time),
                     data= data_period, weights = weights)
    model_period_step <- MASS::stepAIC(period_fit, direction="both", trace = FALSE)
    forecast = c(forecast,predict(model_period_step, newdata = data_test[((i*update_frequency)+1):((i+1)*update_frequency),]))
  }
  for(i in periods){
    data_period = aggregated_data[(i*update_frequency):(rolling_window+i*update_frequency),]
    period_fit <- lm(Load ~(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time)
                     *(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time),
                     data= data_period, weights = weights)
    model_period_step <- MASS::stepAIC(period_fit, direction="both", trace = FALSE)
    forecast = c(forecast,predict(model_period_step, newdata = data_test[((i*update_frequency)+1):test_size,]))
  }
  return(forecast)
}
