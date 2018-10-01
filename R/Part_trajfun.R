#' @title Parturition Trajectory Creation
#
#' @description Creates trajectory variables for ungulate partition prediction.
#' Internally used in parturition methodology.
#' @param data "Cleaned" data
#' @param xname column name for x coordinates
#' @param yname column name for y coordinates
#' @param timename column name for posix time
#' @param idname column name for unique individual identifier
#' @return Resulting object is the original data with trajectory variables attached
#' @keywords part, parturition
#' @export
Part_trajfun<-function(data,timename,xname,yname,idname){
  #change the data name
  sub<-data
  #order the data
  sub<- sub[with(sub, order(sub[,timename])),]
  #run ltraj
  newdata<-adehabitatLT::as.ltraj(sub[,c(xname,yname)],sub[,timename],id=sub[,idname])


  fpt<-adehabitatLT::fpt(newdata,seq(50,1000,50),'hours')[[1]]
  names(fpt)<-c('FPT50','FPT100','FPT150','FPT200','FPT250','FPT300','FPT350','FPT400','FPT450','FPT500',
                'FPT550','FPT600','FPT650','FPT700','FPT750','FPT800','FPT850','FPT900','FPT950','FPT1000')
  #convert ltraj to data frame
  newdata<-adehabitatLT::ld(newdata)
  newdata<-cbind(newdata,fpt[,1:20])
  #return it
  return(newdata)
}
