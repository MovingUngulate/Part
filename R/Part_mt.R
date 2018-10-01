#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param x [[1]][[1]] from modPrep output
#' @param vars output of varPrep
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
Part_mt<-function(x,vars){
  mt<-as.data.frame(randomForest::tuneRF(x=x[,vars],y=as.factor(x[,'birth']), ntreeTry=100,trace=F,plot=F))

  #sort out the tune output to get best parameter
  mt<-mt[order(mt$OOBError),]
  mt<-mt[1,1]
  return(mt)
}
