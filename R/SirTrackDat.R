#' @title Download data from Sirtrack Website
#
#' @description Download all data from a Sirtrack project, save as a csv locally and return a
#' spatialpointsdataframe of the data
#' @param user username used to login to website
#' @param pass password used to login to website
#' @param saveas name of file (as .csv) to save data locally
#' @param yourlink this is the link used to download your data. Login to your sirtrack project,
#' hover over the "Export Full Data Set" csv icon, right click, Copy link address, and paste
#' as a character vector
#' @return Resulting object is a spatial points data frame
#' \strong{Spatial Data Column Description:}
#' \item{CollarSerialNumber}{Collar Serial Number}
#' \item{TelemDate}{POSIX Field of collare fixes in the USER'S timezone. NOTE: may need to alter timezone}
#' \item{HDOP}{Horizontal Dilution of Precision}
#' \item{2D/3D}{Whether fix is a 2d or 3d fix. Values are either 2 or 3}
#' \item{Temperature}{Temperature reported by collar}
#' @keywords sirtrack
#' @export
#' @examples
#' \donttest{stdat<-SirTrackDat(user=email,
#' pass=password,saveas='C:/Users/mhayes1/Desktop/SirtrackDat.csv')}
#'
SirTrackDat<-function(user,pass,saveas,yourlink){
  s<-rvest::html_session('https://data.sirtrack.com/')

  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  curl = RCurl::getCurlHandle()
  params <-
    list(
      'loginEmail' = user,
      'loginPassword' = pass
    )
  html = RCurl::postForm('https://data.sirtrack.com/', .params = params, curl = curl, style="POST")

  fn<-yourlink

  utils::download.file(fn,saveas,quiet=T)

  rn<-read.csv(saveas,stringsAsFactors = F)

  rn$TelemDate<-as.POSIXct(paste(rn$UTC_Date,rn$UTC_Time,sep=' '),'%Y-%m-%d %H:%M:%S',tz='UTC')
  attributes(rn$TelemDate)$tzone<-'MST'

  rn<-rn[,c(2,19,8,9,11,5,6)]

  names(rn)<-c('CollarSerialNumber','TelemDate','HDOP','X2D.3D','Temperature','Lat','Long')
  
  rn$Temperature<-111

  rn$X2D.3D<-as.character(rn$X2D.3D)

  rn<-rn[complete.cases(rn$Long),]
  rn<-rn[complete.cases(rn$Lat),]

  sp::coordinates(rn)<-~Long+Lat
  sp::proj4string(rn)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

  return(rn)
}
