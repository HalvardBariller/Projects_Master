#' Quantile selection
#'
#' Fit multiple smooth additive quantile regression models in order to determine the best quantile level
#'
#' @param train the dataset on which the model is fitted
#' @param cv the cross-validation set
#' @param quantile a vector of quantile levels to compare
#'
#' @return the different forecasts obtained with the quantile levels
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export



quantile_selection <- function(train=Data0[sel_a,],cv=Data0[sel_b,], quantile = seq(0.3,0.5,0.05)){
  equation <- Load~( s(Time,k=3, bs='cr') + s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr')
                     + s(Load.1, bs='cr')+ s(Load.7, bs='cr') + s(Temp_s99,k=10, bs='cr') + WeekDays + BH
                     + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + DLS + te(Temp_s95_min, Temp_s99_min) )
  fit <- qgam::mqgam(equation, data = train, qu = quantile)
  plot(cv$Date, cv$Load, type='l')
  for(iq in quantile){
    pred <- qgam::qdo(fit, iq, predict, newdata = cv)
    lines(cv$Date, pred, col = 2)
  }
}



