#' Naive QGAM
#'
#' Fit a smooth additive quantile regression model based on a light equation
#'
#' @param dataset the dataset on which the model is fitted
#' @param quantile the quantile selected
#'
#' @return the selected model
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


naive_qgam_model <- function(dataset = Data0[sel_a,], quantile = 0.3){
  fit <- qgam::qgam(Load ~ s(Time, k=20, bs="ad")+ s(toy,k=30, bs='cc') + s(Temp,k=10, bs='cr')
                         + s(Load.1, bs='cr')+ s(Load.7, bs='cr') + s(Temp_s99,k=10, bs='cr') + WeekDays + BH
                         + te(Temp_s95_max, Temp_s99_max) + Summer_break  + Christmas_break + te(Temp_s95_min, Temp_s99_min),
                    data = dataset,
                    qu = quantile)
  return(fit)
}
