#' @title Combine ATS and Vectronic Data
#
#' @description This function combines ATS Iridium data and the csv files downloaded by vectronic GPS Plus X
#' @param vecpath Path your Vectronic GPS Plus X Program saves CSV files to
#' @param ATSUsers a character vector of all ATS Iridium login Usernames
#' @param ATSPass a character vector of all ATS Iridium login passwords
#' @param tempdir a temporary directory to store files in when downloading from ATS site
#' @return Resulting object is a spatial points data frame of your ATS and vectronics data
#' downloaded using GPS Plus X as a csv
#' \strong{Spatial Data Column Description:}
#' \item{CollarSerialNumber}{Collar Serial Number}
#' \item{TelemDate}{POSIX Field of collare fixes in the USER'S timezone. NOTE: may need to alter timezone}
#' \item{HDOP}{Horizontal Dilution of Precision}
#' \item{2D/3D}{Whether fix is a 2d or 3d fix. Values are either 2 or 3}
#' \item{Temperature}{Temperature reported by collar}
#' @keywords vectronic, ats
#' @export
#' @examples
#' \donttest{dd<-CombDat(vecpath='F:/Box Sync/DEER/Data/Vectronic/VecData',
#' ATSUsers=c('user1','user2'),
#' ATSPass = c('pass1','pass2'),
#' tempdir='C:/Users/mhayes1/Desktop/temp')}
#'
CombDat<-function(vecpath,ATSUsers,ATSPass,tempdir){

  llist<-list.files(vecpath,
                    pattern='GPS_Default Storage.csv',full.names=T)

  vdat<-data.frame()
  for(i in 1:length(llist)){
    sub<-read.csv(llist[i],stringsAsFactors = F)
    vdat<-rbind(vdat,sub)
  }

  vdat<-vdat[,c(2,3,4,16,17,48,13,14)]
  vdat$TelemDate<-as.POSIXct(paste(vdat$UTC_Date,vdat$UTC_Time,sep=' '),'%m/%d/%Y %H:%M:%S %p',tz='UTC')

  attributes(vdat$TelemDate)$tzone<-'MST'
  vdat<-vdat[,c(1,9,4,5,6,7,8)]
  names(vdat)<-c('CollarSerialNumber','TelemDate','HDOP','X2D.3D','Temperature','Lat','Long')
  vdat<-vdat[complete.cases(vdat$Long),]
  vdat<-vdat[complete.cases(vdat$Lat),]

  
  vdat<-vdat[order(vdat$TelemDate),]
  
  sp::coordinates(vdat)<-~Long+Lat
  sp::proj4string(vdat)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'


  for(i in 1:length(ATSUsers)){
    de<-Part::ColDownload(username=ATSUsers[i],password=ATSPass[i],dirdown=tempdir,cType='ATS/IRID')
    #de[[1]]@data$Study<-ATSList[i]
    if(i == 1){
      dat<-de[[1]]
    }else{
      dat<-rbind(dat,de[[1]])
    }

  }

  #dat<-de[[1]]
  dat@data<-dat@data[,c(1,2,3,5,7)]

  jk<-rbind(dat,vdat)

  st<-SirTrackDat(user='srsdeerproject@gmail.com',pass='wyoming1',
                        saveas=paste0(tempdir,'STDat.csv'),yourlink='https://data.sirtrack.com/serve/project/1910506001/Wyoming%20Range%20Mule%20Deer.csv?key=$2a$10$e.N/8fXpr.0L6RLgTmacN.')
  jk<-rbind(jk,st)
  return(jk)

}
