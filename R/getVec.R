#' @title Vectronic Collar Data Combine
#
#' @description This function combines the csv files downloaded by vectronic GPS Plus X
#' @param vecpath Path your Vectronic GPS Plus X Program saves CSV files to
#' @return Resulting object is a spatial points data frame of your vectronics data
#' downloaded using GPS Plus X as a csv
#' \strong{Spatial Data Column Description:}
#' \item{CollarSerialNumber}{Collar Serial Number}
#' \item{TelemDate}{POSIX Field of collare fixes in the USER'S timezone. NOTE: may need to alter timezone}
#' \item{HDOP}{Horizontal Dilution of Precision}
#' \item{2D/3D}{Whether fix is a 2d or 3d fix. Values are either 2 or 3}
#' \item{Temperature}{Temperature reported by collar}
#' @keywords vectronic
#' @export
#' @examples
#' \donttest{getVec(vecpath='/home/mhayes1/Desktop/Testing/')}
#'
getVec<-function(vecpath){

  llist<-list.files(vecpath,
                    pattern='GPS_Default Storage.csv',full.names=T)

  vdat<-data.frame()
  for(i in 1:length(llist)){
    sub<-read.csv(llist[i],stringsAsFactors = F,encoding='latin1')
    vdat<-rbind(vdat,sub)
  }

  vdat<-vdat[,c(2,3,4,16,17,48,13,14)]
  vdat$TelemDate<-as.POSIXct(paste(vdat$UTC_Date,vdat$UTC_Time,sep=' '),'%m/%d/%Y %I:%M:%S %p',tz='UTC')

  attributes(vdat$TelemDate)$tzone<-'MST'
  vdat<-vdat[,c(1,9,4,5,6,7,8)]
  names(vdat)<-c('CollarSerialNumber','TelemDate','HDOP','X2D.3D','Temperature','Lat','Long')
  vdat<-vdat[complete.cases(vdat$Long),]
  vdat<-vdat[complete.cases(vdat$Lat),]

  sp::coordinates(vdat)<-~Long+Lat
  sp::proj4string(vdat)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

  return(vdat)
}
