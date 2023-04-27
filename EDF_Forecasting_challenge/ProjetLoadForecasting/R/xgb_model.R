#' XGBoost Tree model
#'
#' Fit a XGBoost tree model
#'
#' @param dataset the dataset on which the model is fitted
#'
#' @return the fitted model
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

xgb_tree_model <- function(dataset = Data0[sel_a,]){
  features = c("Load.1","Load.7","Temp","Temp_s95","Temp_s99","Temp_s95_min","Temp_s95_max",
         "Temp_s99_min","Temp_s99_max","toy","Year","Month","Time","BH","Summer_break",
         "Christmas_break","DLS","Date","WeekDays","GovernmentResponseIndex")
  # dtrain <- xgb.DMatrix(data = as.matrix(dataset[features]),label = dataset$Load)
  sparse_matrix <- sparse.model.matrix(Load ~ ., data = dataset)[,-2]
  dtrain = xgb.DMatrix(sparse_matrix, label = dataset$Load)
  params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.2, gamma=0,
                 max_depth=6, min_child_weight=1, subsample=0.7, colsample_bytree=1)
  xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 250, nfold = 5, showsd = T,
                   stratified = T, print_every_n = 10, early_stopping_rounds= 20)
  # fit <- xgb.train(params = params, data = dtrain, nrounds = xgbcv$best_iteration,
                   # watchlist = list(val=dtest, train = dtrain)
                   # print_every_n = 10, early_stopping_rounds = 10)
  fit <- xgboost(params = params, data = dtrain, nrounds = xgbcv$best_iteration,
                   print_every_n = 10, early_stopping_rounds = 10)
  # mat <- xgb.importance(feature_names = features, model = fit)
  # xgb.plot.importance(importance_matrix = mat)
  return(fit)
}


#' XGBoost Linear model
#'
#' Fit a XGBoost linear model
#'
#' @param dataset the dataset on which the model is fitted
#'
#' @return the fitted model
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

xgb_linear_model <- function(dataset = Data0[sel_a,]){
  features = c("Load.1","Load.7","Temp","Temp_s95","Temp_s99","Temp_s95_min","Temp_s95_max",
               "Temp_s99_min","Temp_s99_max","toy","Year","Month","Time")
  dtrain <- xgb.DMatrix(data = as.matrix(dataset[features]),label = dataset$Load)
  xgbcv <- xgb.cv( booster = "gblinear", data = dtrain, nrounds = 10000, nfold = 5, showsd = T,
                   stratified = T, print_every_n = 10, early_stopping_rounds= 20)
  # fit <- xgb.train(params = params, data = dtrain, nrounds = xgbcv$best_iteration,
  # watchlist = list(val=dtest, train = dtrain)
  # print_every_n = 10, early_stopping_rounds = 10)
  fit <- xgboost(booster = "gblinear", data = dtrain, nrounds = xgbcv$best_iteration,
                 print_every_n = 10, early_stopping_rounds = 10)
  mat <- xgb.importance(feature_names = features, model = fit)
  xgb.plot.importance(importance_matrix = mat)
  return(fit)
}



#' XGBoost predict
#'
#' Make predictions on COVID subset of train set and test set based on 
#' XGBoost model trained
#'
#' @param model the XGBoost model trained
#' @param covid_period the COVID subset of train set
#' @param test the test set
#'
#' @return a vector of forecasts
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

xgb_predict <- function(model = xgb_model, covid_period = Data0[sel_b,], test = Data1){
  features = c("Load.1","Load.7","Temp","Temp_s95","Temp_s99","Temp_s95_min","Temp_s95_max",
               "Temp_s99_min","Temp_s99_max","toy","Year","Month","Time","BH","Summer_break",
               "Christmas_break","DLS","Date","WeekDays","GovernmentResponseIndex")
  test_size = nrow(test)
  data_test_reformat = cbind(test$Date,
                             c(test$Load.1[2:test_size],test$Load.1[test_size]),
                             subset(test, select = -c(Id,Date)))
  colnames(data_test_reformat)[1:2] <- c("Date","Load")
  aggregated_data = rbind(covid_period,data_test_reformat)
  aggregated_data = subset(aggregated_data, select = -c(Load))

  sparse_matrix <- sparse.model.matrix(Load ~ ., data = aggregated_data)[,-2]
  dtrain = xgb.DMatrix(sparse_matrix)

  #dpred <- xgb.DMatrix(data = as.matrix(aggregated_data[features]))
  forecast <- predict(model, dpred)
  return(forecast)
}



