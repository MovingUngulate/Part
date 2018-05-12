#' @title BiVariate Gaussian Bridge Stats
#
#' @description Create BiVariate Gaussian Bridge Stats using the move package. Specifically
#' creates parallel and orthogonal variance measures in format to append to data.frame of movement data.
#' This can be a lengthy process and therefore, this code is internally parallelized. You must give an
#' ncpus > 1 to run in parallel.
#' @param data GPS data either as data.frame
#' @param xname name of X coordinate column
#' @param yname name of Y coordinate column
#' @param timename name of POSIX time/date column
#' @param idname name of unique identifier for each animal column
#' @param projstring Correct proj4string for the data/coordinates
#' @param ncpus Number of CPUs for parallel processing. Recommend 1-2 less than max.
#' @param msize Margin size for dynBGB. Defaults to 21.
#' @param winsize Window size for dynBGB. Defaults to 43.
#'
#' @return Resulting object is a data.frame of original data with dynBGB parameters added as columns. See Kraunstauber
#' 2015 for more info.
#'
#' \item{paraSd}{Parallel Variance Measure}
#' \item{orthSd}{orthogonal variance measure}
#' @keywords bivariate gaussian bridge, dynbgb, bgb
#' @export
#' @examples
#' \donttest{BGBFun(data=df, xname='Easting',yname='Northing',timename='TelemDate',idname='Serial',
#' projstring='+proj=longlat +datum=WGS84 +no_defs',ncpus=6,msize=21,winsize=43)}
#'


BGBFun<-function(data,xname,yname,timename,idname,projstring,ncpus,msize=21,winsize=43){
  data <- as.data.frame(data)
  ti<-data
  #need complete cases only and ordered by animal/time
  data<-data[complete.cases(data),]
  data<-data[order(data[,idname],data[,timename]),]
  data$Year<-as.numeric(strftime(data[,timename],format='%Y'))


  #Make move object for dynBGB
  ssub<- move::move(data[,xname], data[,yname],data[,timename],
              proj=sp::CRS(projstring),
              animal=data[,idname])

  #transforming here to Lat/Lon in order to center projection on trajectory
  ssub<- sp::spTransform(ssub, CRSobj=sp::CRS('+proj=longlat +datum=WGS84 +no_defs'))
  spdata<-sp::spTransform(ssub,center=T)
  spdata<-move::split(spdata)
  #create cluster object
  cl <- snow::makeSOCKcluster(rep("localhost", ncpus))

  # give data to cluster
  snow::clusterExport(cl,'spdata',envir=environment())

  snow::clusterEvalQ(cl, library(move))
  
  # calcate movement statistic, split move stack to consider each trajectory seperately
  dBGBvar <- snow::parLapply(cl=cl,x=spdata, fun=move::dynBGBvariance, margin=msize, windowSize=winsize,
                             locErr=21)
  snow::stopCluster(cl)

  outfun<-dBGBvar

  outtra<-data.frame()
  for(i in 1:length(unique(ti$CollarSerialNumber))){
    itraj<-ti[which(ti$CollarSerialNumber==unique(ti$CollarSerialNumber)[i]),]
    #EF: had to add an X to names(outfun) were not matching
    #ffu<-outfun[[which(paste0("X",names(outfun))==unique(ti$id)[i])]]
    ffu<-outfun[[which(gsub('X','',names(outfun))==unique(ti$CollarSerialNumber)[i])]]
    itraj<-itraj[complete.cases(itraj),]
    itraj$Year<-as.numeric(strftime(itraj$TelemDate,format='%Y'))
    #itraj<-itraj[which(itraj$Year==year),]
    itraj$paraSd<-ffu@paraSd
    itraj$orthSd<-ffu@orthSd
    outtra<-rbind(outtra,itraj)
  }
  return(outtra)
}
