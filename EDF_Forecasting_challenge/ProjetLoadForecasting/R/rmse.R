#' Root Mean Square Error
#'
#' compute the Root Mean Square Error between a vector y and its forecast yhat
#'
#' @param y the observations to be predicted
#' @param yhat the predictions
#' @param digits the precision in number of digits
#'
#' @return a positive real number the RMSE
#'
#' @examples
#' y<-rnorm(10)
#' yhat<-rep(0,10)
#' rmse(y,yhat,digits=4)
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


rmse<-function(y,yhat,digits=2){
  return(signif(sqrt(mean((y-yhat)^2,na.rm=TRUE)),digits=digits))
}