#' Adaptative Random Forest model
#'
#' Online learning of Random Forests
#'
#' @param train the data on which we will fit the initial model
#' @param covid_period the subset of train test during COVID
#' @param test the test set
#' @param trees the number of trees in each forest
#' @param var_allowed the number of variables to possibly split at each node
#'
#' @return the forecast on the train COVID period and the test set
#'
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


adaptative_rf_model <- function(train = Data0[sel_a,], covid_period = Data0[sel_b,], test = Data1,
                                trees = 10, var_allowed = 7, rolling_window = 2000){
  train_size = nrow(train)
  test_size = nrow(test)
  data_test_reformat = cbind(test$Date,
                             c(test$Load.1[2:test_size],test$Load.1[test_size]),
                             subset(test, select = -c(Id,Date)))
  colnames(data_test_reformat)[1:2] <- c("Date","Load")
  aggregated_data = rbind(covid_period,data_test_reformat)

  Ntree <- trees
  mtry <- var_allowed
  equation <- Load~ .
  rf<- ranger::ranger(equation, data=train[(train_size-rolling_window):train_size,], num.trees = Ntree, mtry=mtry)
  rf.forecast <-  predict(rf, data=aggregated_data)$predictions
  oob = c()

  for(i in c(1: (nrow(aggregated_data)-1)))
  {
    rf<- ranger::ranger(equation, data=rbind(train[(train_size-rolling_window+i):train_size,], aggregated_data[1:i,]), num.trees = Ntree, mtry=mtry)
    rf.forecast[i+1] <- predict(rf, data=aggregated_data[i+1,], predict.all = F)$predictions
    oob[i] <- rf$prediction.error%>%sqrt
    print(i)
  }
  plot(aggregated_data$Date[-1],oob, type='l')
  return(rf.forecast)
}
