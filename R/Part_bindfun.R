#' @title Parturition Trajectory Binding
#
#' @description Binds trajectory variables with original data for ungulate partition prediction.
#' Internally used in parturition methodology.
#' @param ti trajectory data
#' @param outfun original data
#' @return Binds together trajectory and original data
#' @keywords part, parturition
#' @export
Part_bindfun<-function(ti,outfun){
  outtra<-data.frame()
  for(i in 1:length(unique(ti$id))){
    itraj<-ti[which(ti$id==unique(ti$id)[i]),]
    #EF: had to add an X to names(outfun) were not matching
    #ffu<-outfun[[which(paste0("X",names(outfun))==unique(ti$id)[i])]]
    ffu<-outfun[[which(names(outfun)==unique(ti$id)[i])]]
    itraj$paraSd<-ffu@paraSd
    itraj$orthSd<-ffu@orthSd
    outtra<-rbind(outtra,itraj)
  }
  return(outtra)
}
