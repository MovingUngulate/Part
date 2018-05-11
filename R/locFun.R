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


locFun<-function(vidat,locdat){
  locdat$CollarSerialNumber<-as.character(as.numeric(as.character(locdat$CollarSerialNumber)))
  vidat$CollarSerialNumber<-as.character(as.numeric(vidat$CollarSerialNumber))
  vidat$Date<-as.POSIXct(vidat$Date,format='%m/%d/%Y %H:%M:%S %p')
  #uni<-unique(vidat$CollarSerialNumber)
  
  uni<-unique(vidat$CollarSerialNumber)
  nv<-data.frame()
  for(i in 1:length(uni)){
    sub<-vidat[which(vidat$CollarSerialNumber==uni[i]),]
    sub<-sub[order(sub$Date),]
    sub<-sub[nrow(sub),]
    nv<-rbind(nv,sub)
  }
  
  vidat<-nv
  
  
  plo<-data.frame()
  for(p in 1:nrow(vidat)){
    visu<-vidat[p,]
    locsu<-locdat[which(locdat$CollarSerialNumber==visu$CollarSerialNumber),]
    locsu$DT<-abs(difftime(locsu$TelemDate,visu$Date))
    locsu<-locsu[order(locsu$DT,decreasing=F),]
    locsu<-locsu[1:2,]
    visu$Easting<-mean(locsu$Easting,na.rm=T)
    visu$Northing<-mean(locsu$Northing,na.rm=T)
    
    plo<-rbind(plo,visu)
  }
  return(plo)
}
