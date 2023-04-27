#' Naive QGAM residual variance
#'
#' Observe the residual variance obtained with the naive QGAM and compare it based on temperature levels
#'
#' @param dataset the dataset on which the residuals are observed
#' @param quantile the naive QGAM
#'
#' @return the obtained residuals and variance plots by temperature levels
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


naive_qgam_residual_variance <- function(dataset = Data0[sel_a,], model){
  temp_vector <- rep(0, length(sel_a))
  for (i in 1:length(sel_a)){
    if (dataset$Temp[i]<0){
      temp_vector[i]=0
    }
    if (0<=dataset$Temp[i] & dataset$Temp[i]<5){
      temp_vector[i]=1
    }
    if (5<=dataset$Temp[i] & dataset$Temp[i]<10){
      temp_vector[i]=2
    }
    if (10<=dataset$Temp[i] & dataset$Temp[i]<15){
      temp_vector[i]=3
    }
    if (15<=dataset$Temp[i] & dataset$Temp[i]<20){
      temp_vector[i]=4
    }
    if (20<=dataset$Temp[i] & dataset$Temp[i]<25){
      temp_vector[i]=5
    }
    if (25<=dataset$Temp[i]){
      temp_vector[i]=6
    }
  }
  boxplot(dataset$Load~temp_vector)
  residus_quantile_naive <- dataset$Load-predict(model,newdata = dataset)
  boxplot(residus_quantile_naive~temp_vector)
  return(residus_quantile_naive)
}


