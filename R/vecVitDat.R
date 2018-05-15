#' @title Organize Vectronic Vit Data
#
#' @description Access and organize Vectronic VIT Data to use in daily Fawn Updates
#' @param path path to vectronic data folder
#' @return Resulting object is a data.frame of vectronic VIT data
#' @keywords vectronic, vit
#' @export
#' @examples
#' \donttest{vecVit<-vecVitDat(path='F:/Box Sync/DEER/Data/Vectronic/VecData')}
#'

vecVitDat<-function(path){
  llist<-list.files(path,pattern='VIT_',full.names=T)


  vdat <- data.frame()
  for (i in 1:length(llist)) {
    sub <- read.csv(llist[i], stringsAsFactors = F,encoding='latin1')
    vdat <- rbind(vdat, sub)
  }

  vdat <- vdat[, c(2, 3, 4, 12)]
  vdat$TelemDate <- as.POSIXct(paste(vdat$UTC.Date, vdat$UTC.Time,
                                     sep = " "), "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  attributes(vdat$TelemDate)$tzone <- "MST"
  vdat <- vdat[, c(1, 5, 4)]
  names(vdat) <- c("CollarSerialNumber", "TelemDate", 'VITStatus')


  vdat$NumFixes<-NA
  vdat$BattVoltage<-NA
  vdat$Mortality<-NA
  vdat$BreakOff<-NA
  vdat$GPSOnTime<-NA
  vdat$SatOnTime<-NA
  vdat$SatErrors<-NA
  vdat$GMTOffset<-NA
  vdat$LowBatt<-NA
  vdat$Da<-NA
  vdat$Ti<-NA
  vdat$PM<-NA
  vdat$Month<-strftime(vdat$TelemDate,format='%m')
  vdat$Day<-strftime(vdat$TelemDate,format='%d')
  vdat$Year<-strftime(vdat$TelemDate,format='%Y')



  names(vdat)[2]<-'Date'
  names(vdat)[3]<-'NeoLink Status'

  vdat$Date<-as.character(strftime(vdat$Date,format='%m/%d/%Y %H:%M:%S %p'))

  vdat<-vdat[,c('CollarSerialNumber','Date','NumFixes','BattVoltage','Mortality',
                'BreakOff','GPSOnTime','SatOnTime','SatErrors','GMTOffset','LowBatt',
                'NeoLink Status','Da','Ti','PM','Month','Day','Year')]

  return(vdat)
}
