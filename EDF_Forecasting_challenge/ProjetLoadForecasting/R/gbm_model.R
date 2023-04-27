#' Generalized Boosted Regresison Modeling
#'
#' Fit a linear model to the data, constructing polynomial combinations of the features
#' and choosing the proper model by AIC in bidirectional stepwise algorithm
#'
#' @param dataset the dataset on which the model is fitted
#'
#' @return the selected model
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


gbm_model <- function(dataset=Data0[sel_a,], distribution){
  eq <- Load ~ Load.1 + Load.7 + Temp + Temp_s95 + Temp_s95_max + Temp_s99_max +
    toy + WeekDays + BH + DLS + Summer_break + Christmas_break +
    Time + Load.1:Temp + Load.1:Temp_s95 + Load.1:Temp_s95_max +
    Load.1:Temp_s99_max + Load.1:toy + Load.1:WeekDays + Load.1:DLS +
    Load.1:Summer_break + Load.1:Christmas_break + Load.7:Temp_s95_max +
    Load.7:Temp_s99_max + Load.7:DLS + Load.7:Summer_break +
    Load.7:Christmas_break + Temp:Temp_s95 + Temp:Temp_s99_max +
    Temp:WeekDays + Temp:DLS + Temp:Christmas_break +
    Temp_s95:Temp_s95_max + Temp_s95:Temp_s99_max +
    Temp_s95:toy + Temp_s95:WeekDays +
    Temp_s95:DLS + Temp_s95:Summer_break + Temp_s95:Christmas_break +
    Temp_s95:Time + Temp_s95_max:Temp_s99_max + Temp_s95_max:toy +
    Temp_s95_max:Christmas_break + Temp_s95_max:Time +
    Temp_s99_max:toy +Temp_s99_max:WeekDays + Temp_s99_max:BH +
    Temp_s99_max:DLS + toy:WeekDays + toy:BH + toy:DLS +
    toy:Summer_break + toy:Christmas_break +
    WeekDays:BH + WeekDays:DLS + WeekDays:Summer_break +
    WeekDays:Christmas_break +
    WeekDays:Time + BH:DLS + BH:Summer_break + BH:Christmas_break +
    BH:Time + Summer_break:Time
  weights <- (dataset$GovernmentResponseIndex*.05)+1
  fit <- gbm::gbm(eq, data = dataset, weights = weights, interaction.depth = 15,
                  distribution = distribution, n.minobsinnode = 5, n.trees = 250,
                  shrinkage = 0.05)
  trees <- gbm::gbm.perf(fit, method = "OOB")
  print(trees)
  fit2 <- gbm::gbm(eq, data = dataset, weights = weights, interaction.depth = 15,
                  distribution = distribution, n.minobsinnode = 5, n.trees = trees,
                  shrinkage = 0.05)
  return(fit2)
}
