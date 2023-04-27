#' Feature Engineering
#'
#' Operates diverse basic transformations on datasets
#'
#' @param dataset the dataset on which the transformations are applied
#'
#' @return the modified dataset
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


feature_engineering <- function(dataset){
  dataset$Time <- as.numeric(dataset$Date)
  dataset$WeekDays=factor(dataset$WeekDays)
  dataset$BH=factor(dataset$BH)
  dataset$DLS=factor(dataset$DLS)
  dataset$Summer_break=factor(dataset$Summer_break)
  dataset$Christmas_break=factor(dataset$Christmas_break)
  return(dataset)
}
