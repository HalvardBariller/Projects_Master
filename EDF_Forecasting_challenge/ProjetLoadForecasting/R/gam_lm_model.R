#' GAM + LM
#'
#' Fit a Generalized Additive model, time-weighted or not, adjusted with features constructed for the linear model
#'
#' @param model the dataset for training
#' @param weighted logical used to determine if we fit a time-weighted GAM or not. If true, use exponentially decreasing weights.
#'
#' @return the fitted model
#'
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export




gam_lm_model <- function(dataset = Data0[sel_a,], weighted =T){
  weights = rep(1,nrow(dataset))
  if (weighted == T){
    weights = exp(dataset$Time/10000)
  }
  fit <- gam(formula = Load ~
               Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
               toy + WeekDays + BH + DLS + Summer_break + Christmas_break + Time+
               Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
               Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays + Load.1:DLS +
               Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
               Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
               Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
               Temp:WeekDays + Temp:DLS + Temp:Christmas_break + Temp_s95:Temp_s95_max +
               Temp_s95:Temp_s99_max + Temp_s95:toy + Temp_s95:WeekDays +
               Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
               Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
               Temp_s95_max:Christmas_break + Temp_s95_max:Time + Temp_s99_max:toy +
               Temp_s99_max:WeekDays + Temp_s99_max:BH + Temp_s99_max:DLS +
               toy:WeekDays + toy:BH + toy:DLS + toy:Summer_break + toy:Christmas_break +
               WeekDays:BH + WeekDays:DLS + WeekDays:Summer_break + WeekDays:Christmas_break +
               WeekDays:Time + BH:DLS + BH:Summer_break + BH:Christmas_break +
               BH:Time + Summer_break:Time
             +s(Temp, k = 50, bs = "cr") + s(toy, k = 50, bs ="cr")
             +s(Temp_s95, k = 50, bs ="cr")
             +s(Temp_s95_min, k = 50, bs ="cr")
             +s(toy, k = 30, bs ="cr", by=WeekDays)+WeekDays
             +s(Temp, k = 30, bs = "cr", by=WeekDays)
             ,select=TRUE
             ,data = dataset,
             weights = weights)
  return(fit)
}
