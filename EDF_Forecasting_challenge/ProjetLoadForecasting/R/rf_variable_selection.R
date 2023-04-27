#' Variable selection with Random Forest
#'
#' Adjust a random forest so as to visualize
#'
#' @param dataset the data on which we will fit the model
#' @param var_allowed the number of variables to possibly split at each node
#' @param trees the number of trees in the forest
#'
#' @return the vector of variable importance
#'
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

rf_variable_selection <- function(dataset = Data0[sel_a,], var_allowed = 13, trees = 200){
  equation <- Load~.
  rf <- ranger(equation, data=dataset, importance =  'permutation', mtry = var_allowed, num.trees = trees)
  imp <- rf$variable.importance
  o <- order(imp, decreasing=T)
  p <- imp[o]
  return(p)
}
