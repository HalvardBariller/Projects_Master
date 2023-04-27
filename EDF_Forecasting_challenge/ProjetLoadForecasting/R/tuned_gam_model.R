#' Tuned GAM
#'
#' adjust a GAM based on tuned equation
#'
#' @param dataset the data on which we will fit the model

#'
#' @return the fitted GAM
#'
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export



gam_tuned_model <- function(dataset=Data0[sel_a,]){
  equation <- Load~( WeekDays + BH + Christmas_break + Summer_break + DLS
                      + s(Temp_s99_max, k = 5, bs='cr',by=WeekDays)
                      + s(Temp,k = 5, bs='cr',by=WeekDays)
                      #+ s(Temp_s99,k = 5, bs='cr',by=WeekDays)
                      + s(Load.1,k = 5, bs='cr',by=WeekDays)
                      + s(toy,k = 50, bs='cr')
                      + s(Time, k=15, bs='cr')
                      + s(Load.7, bs='cr',by=WeekDays)
                      + te(Temp_s99_max, Temp_s99_min)
                      + te(Temp_s95_max, Temp_s95_min))
  fit <- gam(formula = equation, data=dataset ,select = T)
  return(fit)
}
