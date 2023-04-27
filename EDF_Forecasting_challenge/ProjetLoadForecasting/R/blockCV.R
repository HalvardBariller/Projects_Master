#' Build_block
#'
#' Build blocks of data prior to leave-one-out cross-validation
#' 
#' @param dataset the dataset used for cross-validation
#' @param Nblock the number of folds used
#'
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export


build_block <- function(dataset = Data0[sel_a], Nblock = 8){
  borne_block<-seq(1, nrow(dataset), length=Nblock)%>%floor
  block_list<-list()
  l<-length(borne_block)
  for(i in c(2:(l-1))){
    block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
  }
  block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))
  return(block_list)
}


#' Linear model on block
#'
#' Adjust linear model on block for CV
#' 
#' @param eq the equation for the model
#' @param block a vector with the indexes of the row used for CV 
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

fit_lm_mod <- function(eq, block)
{
  mod <- lm(eq, data=Data0[-block,])
  mod.cvpred <- predict(mod, newdata=Data0[block,])
  return(mod.cvpred)
}

#' Linear model block Cross-Validation 
#'
#' Leave-one-out K-fold cross-validation to evaluate LM performance on the training set subset anterior to COVID
#'
#'@param dataset train set
#'@param eq the equation used for the model
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

lm_cvpred <- function(dataset=Data0[sel_a,],eq){
  cvpred<-lapply(build_block(dataset = dataset), fit_lm_mod, eq=eq)%>%unlist
  return(cvpred)
}


#' GAM on block
#'
#' Adjust GAM on block for CV
#' 
#' @param equation the equation for the model
#' @param block a vector with the indexes of the row used for CV 
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

fit_gam_mod<-function(equation, block)
{
  g<- gam(as.formula(equation), data=Data0[-block,])
  forecast<-predict(g, newdata=Data0[block,])
  return(forecast)
}

#' GAM block Cross-Validation 
#'
#' Leave-one-out K-fold cross-validation to evaluate GAM performance on the training set subset anterior to COVID
#'
#'@param dataset train set
#'@param equation the equation used for the model
#'
#' @author halvard.bariller@universite-paris-saclay.fr>
#' @export

gam_cvpred <- function(dataset = Data0[sel_a,], equation){
  Block_forecast<-lapply(build_block(dataset = dataset), fit_gam_mod, equation)%>%unlist
  return(Block_forecast)
}

