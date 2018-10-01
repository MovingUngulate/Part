#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param prep output of modprep
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
Part_varPrep<-function(prep){
  boots = data.frame()
  for(j in 1:length(prep)){
    a<-as.data.frame(prep[[j]][2])
    colnames(a)<-'vars'
    a$vars<-as.character(a$vars)
    boots = rbind(boots, a)
  }

  tab<-as.data.frame(table(boots$vars))
  tab<-tab[which(tab$Freq>=max(tab$Freq)*0.4),1]
  tab<-as.character(tab)
  #tabout<-tab$Var1
  return(tab)
}
