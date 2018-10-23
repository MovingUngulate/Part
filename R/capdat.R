#' @title Conversion of GPS data to GPX/KML for use in capture events.
#
#' @description Convert your spatial data into GPX and KML files for use in captures. Noteably, used
#' for helicopter recaptures of previously marked animals. Takes input data, finds last fix for each animal
#' and converts this data to a GPX and KML file. Also saves a csv with the same data so user can inspect
#' the data and ensure that last fix occured recently. Helpful to determine dead or failed collars.
#' @param data data.frame of relocation data. Should contain unique ID field, POSIXct time/date,
#' x coordinate (column name must be 'x') and y coordinate (column name must be 'y').
#' @param uid Unique animal identifier column header, frequency or serial.
#' This is what will show up on GPS/KML. Likely want this to be column containing frequency.
#' @param keepvec (OPTIONAL) Character vector of animals to keep
#' @param folder Location to save gpx/kml file to. (eg. 'C:/Users/mhayes1/Desktop/')
#' @param TD Column name for POSIX time/date field.
#' @param proj proj4string of Easting/Northing values. Can be obtained by running proj4string(Irid[[1]])
#' on output of Irid function or running proj4string on the SpatialPointsDataFrame.
#' @param spp (OPTIONAL) This is the layer name of the kml as viewed in Google Earth
#' @return Saves a .gpx, .kml and .csv file of most recent relocation for each animal in 'data'. Can be used
#' directly with output of GStar or Irid after converting them to data.frames.
#' @keywords capture, gpx, kml
#' @export
#' @examples
#' \donttest{ark<-GStar(username=username,password=password)

#'a<-ark[[1]]
#'proj<-proj4string(a)
#'a<-as.data.frame(a)
#'names(a)[3:4]<-c('y','x')


#'capdat(data=a,uid='CollarSerialNumber',fold='C:/Users/mhayes1/Desktop/test/',
#'       TD='TelemDate',proj=proj,spp='Deers')}
#'

capdat<-function(data,uid,keepvec=NULL,folder,TD,proj,spp='animal'){
  
  #data<-spTransform(data,'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
  #uniD<-unique(data[,uid])
  
  if(is.null(keepvec)){
    uniD<-unique(data[,uid])
  }else{
    uniD<-keepvec
    
  }
  
  outdata<-data.frame()
  for(i in 1:length(uniD)){
    sub<-data[which(data[,uid]==uniD[i]),]
    
    subM<-sub[which(sub[,TD]==max(sub[,TD])),]
    subM<-as.data.frame(subM)
    
    outdata<-rbind(outdata,subM)
    # }
    
  }
  
  
  coordinates(outdata)<-~x+y
  proj4string(outdata)<-proj
  
  
  outdata<-spTransform(outdata,'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
  fold1<-paste(folder,'CapLocs.kml',sep='')
  fold2<-paste(folder,'CapLocs.gpx',sep='')
  fold3<-paste(folder,'CapLocs.csv',sep='')
  
  
  #outdata$name<-as.character(outdata$New.Frequency)
  
  #namesm<-names(outdata@data)
  
  
  names(outdata)[names(outdata)==uid] <- "name"
  
  write.csv(outdata,fold3,row.names=F)
  
  writeOGR(outdata['name'], fold1, layer=spp, driver="KML",overwrite=T)
  
  writeOGR(outdata['name'], dsn=fold2,layer=spp,driver='GPX',overwrite=T)
  
}
