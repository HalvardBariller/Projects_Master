#' Linear model with AIC penalty
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


linear_model_AIC <- function(dataset = Data0[sel_a,]){
  #eq <- paste("Load ~ (",paste(colnames(dataset)[-(1:2)],collapse = "+"),")*(",paste(colnames(dataset)[-(1:2)],collapse = "+"),")")
  eq = (Load ~(Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time)*
          (Load.1+Load.7+Temp+Temp_s95+Temp_s95_max+Temp_s99_max+toy+WeekDays+BH+DLS+Summer_break+Christmas_break+GovernmentResponseIndex+Time))
  fit <- lm(eq,data=dataset)
  step <- stepAIC(fit,direction = "both", trace = FALSE)
  return(step)
}

