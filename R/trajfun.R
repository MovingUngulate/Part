#' @title Movement Trajectory
#
#' @description Use adehabitatLT to create trajectory information for movement data and append the columns
#' to input spatial data. Internally called function; should not be used on it's own.
#' @param data data.frame containing GPS data
#' @param timename name of POSIX field
#' @param xname name of x coordinate field
#' @param yname name of y coordinate field
#' @param idname name of individual identifier field
#' @export

trajfun<-function(data,timename,xname,yname,idname){
  #change the data name
  sub<-data
  #order the data
  sub<- sub[with(sub, order(sub[,timename])),]
  #run ltraj
  newdata<-adehabitatLT::as.ltraj(sub[,c(xname,yname)],sub[,timename],id=sub[,idname])
  #convert ltraj to data frame
  newdata<-adehabitatLT::ld(newdata)
  #return it
  return(newdata)
}
