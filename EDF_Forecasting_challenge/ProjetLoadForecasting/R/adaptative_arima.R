#' Adaptative Arima
#'
#' adjust an initial ARIMA model based on training data, then updates the ARIMA model
#' on a regular time basis based on previously observed residuals in test data
#'
#' @param model the model on which we will apply an ARIMA correction on the residuals
#' @param data_train the initial dataset on which we adjust an ARIMA model
#' @param data_test the dataset used to update the ARIMA model
#' @param update_frequency the number of passed days before readjusting the ARIMA model
#' @param rolling_window the length of the residuals vector with which the ARIMA will be adjusted
#'
#' @return a vector of 'Load' predictions based on forecasts obtained with the considered model
#' corrected with the ARIMA sequentially fitted
#'
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


adaptative_arima<-function(model, data_train = Data0[sel_b,], data_test = Data1,
                               update_frequency = 20, rolling_window = 400){
  test_size = nrow(data_test)
  periods <- floor(test_size/update_frequency)-1
  size_last_period <- test_size%%update_frequency

  model.forecast<- predict(model,newdata = data_train)
  covid_residuals <- data_train$Load-model.forecast
  covid_residuals.ts <- ts(covid_residuals, frequency=7)
  #initial ARIMA model based on train set
  fit.arima.res <- auto.arima(covid_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2,
                              trace=F,ic="aic", method="CSS",stepwise=FALSE,approximation=FALSE)
  Load.model_forecast <- predict(model, newdata=data_test)
  ts_res_forecast <- ts(c(covid_residuals.ts ,data_test$Load.1[2:test_size]-Load.model_forecast[1:(test_size-1)]),  frequency= 7)
  refit <- Arima(ts_res_forecast, model=fit.arima.res)
  prevARIMA.res <- tail(refit$fitted, (test_size-1))

  test.arima.forecast <- Load.model_forecast + c(prevARIMA.res,prevARIMA.res[(test_size-1)])

  updated_residuals.ts = covid_residuals.ts
  updated_forecasts = test.arima.forecast[1:update_frequency]

  for(i in 0:(periods-1)){
    ## on récupère résidus sur la période passée
    new_residuals = (data_test$Load.1[(i*update_frequency+2):((i+1)*update_frequency+1)]
                     -Load.model_forecast[(i*update_frequency+1):((i+1)*update_frequency)])
    updated_residuals_length = length(c(updated_residuals.ts,new_residuals))
    if(updated_residuals_length>rolling_window){
      updated_residuals.ts <- ts(c(updated_residuals.ts,new_residuals)[(updated_residuals_length-rolling_window):updated_residuals_length],
                                 frequency=7)
    } else {
      updated_residuals.ts <- ts(c(updated_residuals.ts,new_residuals), frequency=7)
    }
    updated_arima.res <- auto.arima(updated_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2,
                                    trace=F,ic="aic", method="CSS",stepwise=FALSE,approximation=FALSE)
    period_forecast = ts(data_test$Load.1[((i+1)*update_frequency+2):((i+2)*update_frequency+1)]
                         -Load.model_forecast[((i+1)*update_frequency+1):((i+2)*update_frequency)],  frequency= 7)
    re_updated <- Arima(period_forecast, model=updated_arima.res)
    updated_forecasts = c(updated_forecasts,Load.model_forecast[((i+1)*update_frequency+1):((i+2)*update_frequency)]+re_updated$fitted)
  }
  for(i in periods){
    new_residuals = (data_test$Load.1[(i*update_frequency+2):((i+1)*update_frequency+1)]
                     -Load.model_forecast[(i*update_frequency+1):((i+1)*update_frequency)])
    updated_residuals_length = length(c(updated_residuals.ts,new_residuals))
    if(updated_residuals_length>rolling_window){
      updated_residuals.ts <- ts(c(updated_residuals.ts,new_residuals)[(updated_residuals_length-rolling_window):updated_residuals_length],
                                 frequency=7)
    } else {
      updated_residuals.ts <- ts(c(updated_residuals.ts,new_residuals), frequency=7)
    }
    updated_arima.res <- auto.arima(updated_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2,
                                    trace=F,ic="aic", method="CSS",stepwise=FALSE,approximation=FALSE)
    period_forecast = ts(data_test$Load.1[((i+1)*update_frequency+2):test_size]
                         -Load.model_forecast[((i+1)*update_frequency+1):(test_size-1)],  frequency= 7)
    re_updated <- Arima(period_forecast, model=updated_arima.res)
    updated_forecasts = c(updated_forecasts, Load.model_forecast[((i+1)*update_frequency+1):(test_size-1)]+re_updated$fitted)
  }
  updated_forecasts = c(updated_forecasts,updated_forecasts[(test_size-1)])
  return(updated_forecasts)
}








#' Adaptative Arima on forecasts
#'
#' adjust an initial ARIMA model based on training data, then updates the ARIMA model
#' on a regular time basis based on previously observed residuals in test data with forecasts as inputs
#' instead of model
#'
#' @param train_forecast_covid the predictions obtained with a model on the COVID subset of the train set
#' @param test_forecast the predictions obtained with a model on the test set
#' @param data_train the initial dataset on which we adjust an ARIMA model
#' @param data_test the dataset used to update the ARIMA model
#' @param update_frequency the number of passed days before readjusting the ARIMA model
#' @param rolling_window the length of the residuals vector with which the ARIMA will be adjusted
#'
#' @return a vector of 'Load' predictions based on forecasts obtained with the considered model
#' corrected with the ARIMA sequentially fitted
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


adaptative_arima_on_forecast<-function(train_forecast_covid, test_forecast, data_train = Data0[sel_b,], data_test = Data1,
                           update_frequency = 20, rolling_window = 400){
  test_size = nrow(data_test)
  periods <- floor(test_size/update_frequency)-1
  size_last_period <- test_size%%update_frequency

  model.forecast<- train_forecast_covid
  covid_residuals <- data_train$Load-model.forecast
  covid_residuals.ts <- ts(covid_residuals, frequency=7)
  #initial ARIMA model based on train set
  fit.arima.res <- auto.arima(covid_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2,
                              trace=F,ic="aic", method="CSS",stepwise=FALSE,approximation=FALSE)
  Load.model_forecast <- test_forecast
  ts_res_forecast <- ts(c(covid_residuals.ts ,data_test$Load.1[2:test_size]-Load.model_forecast[1:(test_size-1)]),  frequency= 7)
  refit <- Arima(ts_res_forecast, model=fit.arima.res)
  prevARIMA.res <- tail(refit$fitted, (test_size-1))

  test.arima.forecast <- Load.model_forecast + c(prevARIMA.res,prevARIMA.res[(test_size-1)])

  updated_residuals.ts = covid_residuals.ts
  updated_forecasts = test.arima.forecast[1:update_frequency]

  for(i in 0:(periods-1)){
    ## on récupère résidus sur la période passée
    new_residuals = (data_test$Load.1[(i*update_frequency+2):((i+1)*update_frequency+1)]
                     -Load.model_forecast[(i*update_frequency+1):((i+1)*update_frequency)])
    updated_residuals_length = length(c(updated_residuals.ts,new_residuals))
    if(updated_residuals_length>rolling_window){
      updated_residuals.ts <- ts(c(updated_residuals.ts,new_residuals)[(updated_residuals_length-rolling_window):updated_residuals_length],
                                 frequency=7)
    } else {
      updated_residuals.ts <- ts(c(updated_residuals.ts,new_residuals), frequency=7)
    }
    updated_arima.res <- auto.arima(updated_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2,
                                    trace=F,ic="aic", method="CSS",stepwise=FALSE,approximation=FALSE)
    period_forecast = ts(data_test$Load.1[((i+1)*update_frequency+2):((i+2)*update_frequency+1)]
                         -Load.model_forecast[((i+1)*update_frequency+1):((i+2)*update_frequency)],  frequency= 7)
    re_updated <- Arima(period_forecast, model=updated_arima.res)
    updated_forecasts = c(updated_forecasts,Load.model_forecast[((i+1)*update_frequency+1):((i+2)*update_frequency)]+re_updated$fitted)
  }
  for(i in periods){
    new_residuals = (data_test$Load.1[(i*update_frequency+2):((i+1)*update_frequency+1)]
                     -Load.model_forecast[(i*update_frequency+1):((i+1)*update_frequency)])
    updated_residuals_length = length(c(updated_residuals.ts,new_residuals))
    if(updated_residuals_length>rolling_window){
      updated_residuals.ts <- ts(c(updated_residuals.ts,new_residuals)[(updated_residuals_length-rolling_window):updated_residuals_length],
                                 frequency=7)
    } else {
      updated_residuals.ts <- ts(c(updated_residuals.ts,new_residuals), frequency=7)
    }
    updated_arima.res <- auto.arima(updated_residuals.ts,max.p=3,max.q=4, max.P=2, max.Q=2,
                                    trace=F,ic="aic", method="CSS",stepwise=FALSE,approximation=FALSE)
    period_forecast = ts(data_test$Load.1[((i+1)*update_frequency+2):test_size]
                         -Load.model_forecast[((i+1)*update_frequency+1):(test_size-1)],  frequency= 7)
    re_updated <- Arima(period_forecast, model=updated_arima.res)
    updated_forecasts = c(updated_forecasts, Load.model_forecast[((i+1)*update_frequency+1):(test_size-1)]+re_updated$fitted)
  }
  updated_forecasts = c(updated_forecasts,updated_forecasts[(test_size-1)])
  return(updated_forecasts)
}



