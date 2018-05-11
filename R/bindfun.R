#' @title Bind Movement Trajectory and GPS Data
#
#' @description Combines output of movement trajectory creation and GPS data into a single
#' data.frame. Internally called function; should not be used on it's own.
#' @param ti data.frame output from adehabitatLT as.ltraj function. Also the output from trajfun
#' @param data data.frame used to make ti
#' @param spp Species of animal
#' @keywords bind
#' @export

bindfun<-function(ti,data,spp='Elk'){
  outtra<-data.frame()
  for(i in 1:length(unique(ti$id))){
    itraj<-ti[which(ti$id==unique(ti$id)[i]),]
    #EF: had to add an X to names(outfun) were not matching
    #ffu<-outfun[[which(paste0("X",names(outfun))==unique(ti$id)[i])]]
    ffu<-data[which(data$CollarSerialNumber==unique(ti$id)[i]),]
    #     itraj$paraSd<-ffu@paraSd
    #     itraj$orthSd<-ffu@orthSd
    itraj<-cbind(ffu,itraj)
    #if('Activity' %in% names(ffu)){
    itraj<-itraj[,c(1:5,8,9,13:19)]
    itraj$Species<-spp
    # }else{
    #   itraj<-itraj[,c(1:7,11:17)]
    #   itraj$Species<-'Deer'
    # }
    outtra<-rbind(outtra,itraj)
  }
  return(outtra)
}
