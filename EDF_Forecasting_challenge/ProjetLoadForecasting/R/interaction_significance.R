#' Interaction significance
#'
#' Perform a Chi-squared test between slotted models to assess the significance
#' of an interaction between two features
#'
#' @param dataset the data on which the models are fitted
#' @param feature1 the first feature to test
#' @param feature2 the second feature to test
#'
#' @return the p-value of a Chi-squared test on slotted models
#'
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

interaction_significance <- function(dataset = Data0[sel_a,], feature1, feature2){
  eq_int_1 <- paste("Load~ ti(", feature1, ", bs='cr')")
  eq_int_2 <- paste("Load~ ti(", feature1, ", bs='cr')+ ti(", feature2, ", bs='cr') + ti(", feature1, ", ",feature2,")")
  fit1<-gam(as.formula(eq_int_1), data=dataset)
  fit2<-gam(as.formula(eq_int_2), data=dataset)
  return(anova(fit1, fit2, test = "Chisq")$Pr)
}

