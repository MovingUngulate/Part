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


tabFun<-function(vhist,vi,viout,outtra,spp='deer'){
  if(spp=='deer'){
    vi<-vi[which(as.character(as.numeric(vi$CollarSerialNumber)) %in% vhist$Serial.Number),]
    cursas<-vi
    cs<-data.frame()
    uni<-unique(cursas$CollarSerialNumber)
    for(i in 1:length(uni)){
      s<-cursas[which(cursas$CollarSerialNumber==uni[i]),]  
      s<-s[which(s$Date==max(s$Date)),]
      cs<-rbind(cs,s)
    }
    
    # if(spp=='deer'){
    #   ntab<-vhist[,c(1,8,9,3,4,5,2)]
    # }else{
    ntab<-vhist
    # }
    ntab$Mortality<-NA
    
    
   # ntab<-ntab[which(!(ntab$Serial.Number %in% mlist)),]
    
    vidat<-vi
    vidat$CollarSerialNumber<-as.character(as.numeric(as.character(vidat$CollarSerialNumber)))
    uni<-unique(vidat$CollarSerialNumber)
    agg<-data.frame()
    for(l in 1:length(uni)){
      s<-vidat[which(vidat$CollarSerialNumber==uni[l]),]
      s<-s[complete.cases(s$Event),]
      if(nrow(s)==0){next}
      if(nrow(s)==1){
        count<-0
        jk<-data.frame(CollarSerialNumber=s$CollarSerialNumber[1],
                       VitStatusChanges=count,stringsAsFactors = F)
        agg<-rbind(agg,jk)
      }else{
        count<-0
        for(y in 2:nrow(s)){
          
          if(s$Event[y]==s$Event[y-1]){
            next
          }else{
            count<-count+1
          }
        }
        jk<-data.frame(CollarSerialNumber=s$CollarSerialNumber[1],
                       VitStatusChanges=count,stringsAsFactors = F)
        agg<-rbind(agg,jk)
      }
    }
    # 
    # vidat$n<-ifelse(vidat$Event=='Birth-Not yet triggered',0,1)
    # #ifelse(vidat$Event=='Birth-triggered by lack of comm',1,2
    # #))
    # 
    # agg<-aggregate(vidat$n,by=list(vidat$CollarSerialNumber),FUN=sum,na.rm=T)
    # colnames(agg)<-c('CollarSerialNumber','VitStatusChanges')
    # agg$CollarSerialNumber<-as.numeric(as.character(as.numeric(agg$CollarSerialNumber)))
    
    ntab<-merge(ntab,agg,by.x='Serial.Number',by.y='CollarSerialNumber')
    
    
    visub<-vi
    visub$n<-ifelse(visub$Event=='Birth-Not yet triggered',0,1)
    md<-Sys.time()
    md<-md-(86400*10)
    
    #visub$Date<-as.POSIXct(visub$Date,format='%m/%d/%Y')
    
    #visub<-visub[which(visub$Date>=md),]
    
    visub$CollarSerialNumber<-as.character(as.numeric(as.character(visub$CollarSerialNumber)))
    uni<-unique(visub$CollarSerialNumber)
    agg<-data.frame()
    for(l in 1:length(uni)){
      s<-visub[which(visub$CollarSerialNumber==uni[l]),]
      visub<-visub[which(visub$Date>=md),]
      s<-s[complete.cases(s$Event),]
      if(nrow(s)==0){
        jk<-data.frame(CollarSerialNumber=uni[l], VitStatusChanges_10Day=0,stringsAsFactors = F)
        agg<-rbind(agg,jk)
        next
      }
      if(nrow(s)==1){
        count<-0
        jk<-data.frame(CollarSerialNumber=s$CollarSerialNumber[1],
                       VitStatusChanges_10Day=count,stringsAsFactors = F)
        agg<-rbind(agg,jk)
        next
      }else{
        count<-0
        for(y in 2:nrow(s)){
          
          if(s$Event[y]==s$Event[y-1]){
            next
          }else{
            count<-count+1
          }
        }
        jk<-data.frame(CollarSerialNumber=s$CollarSerialNumber[1],
                       VitStatusChanges_10Day=count,stringsAsFactors = F)
        agg<-rbind(agg,jk)
      }
    }
    
    
    #agg$CollarSerialNumber<-as.numeric(as.character(as.numeric(agg$CollarSerialNumber)))
    
    ntab<-merge(ntab,agg,by.x='Serial.Number',by.y='CollarSerialNumber')
    
    
    viots<-viout
    
    
    #viots<-viots[which(!(viots$CollarSerialNumber %in% mlist)),]
    viots<-viots[complete.cases(viots$CollarSerialNumber),]
    uni<-unique(viots$CollarSerialNumber)
    
    tl<-data.frame()
    for(i in 1:length(uni)){
      vsub<-viots[which(viots$CollarSerialNumber==uni[i]),]
      
      
      if(nrow(vsub)<2){
        vsub<-viots[which(viots$CollarSerialNumber==uni[i]),]
        #v<-vsub[which(vsub$Date==max(vsub$Date,na.rm=T)),]
        
        tl<-rbind(tl,vsub)
        
        next
      }
      
      vsub<-viots[which(viots$CollarSerialNumber==uni[i]),]
      v<-vsub[which(vsub$Date==max(vsub$Date,na.rm=T)),]
      
      vsub<-vsub[which(!(vsub$Event==v$Event[1])),]
      
      if(nrow(vsub)>=2){
        
        vsub<-viots[which(viots$CollarSerialNumber==uni[i]),]
        v<-vsub[which(vsub$Date==max(vsub$Date,na.rm=T)),]
        
        vsub<-vsub[which(!(vsub$Event==v$Event[1])),]
        
        vsub<-vsub[which(vsub$Date==max(vsub$Date,na.rm=T)),]
        
        v$Easting<-vsub$Easting
        v$Northing<-vsub$Northing
        
        tl<-rbind(tl,v)
        
        next
      }
      
    }
    tl<-tl[,c(1,19:20)]
    tl$CollarSerialNumber<-as.numeric(tl$CollarSerialNumber)
    
    ntab<-merge(ntab,tl,by.x='Serial.Number',by.y='CollarSerialNumber')
    
    cs<-cs[,c(1,12)]
    cs$CollarSerialNumber<-as.character(as.numeric(cs$CollarSerialNumber))
    #if(spp=='deer'){
    
    ntab<-merge(ntab,cs,by.x='Serial.Number',by.y='CollarSerialNumber',all.x=T)
    
    if(!('AID' %in% names(ntab))){
      colnames(ntab)[9:11]<-c('EventEasting','EventNorthing','CurrentStatus')
    }
    if('AID' %in% names(ntab)){
      colnames(ntab)[10:12]<-c('EventEasting','EventNorthing','CurrentStatus')
    }
    # }else{
    #   colnames(ntab)[8:10]<-c('CurrentStatus','EventEasting','EventNorthing')
    # }
    # 
    locs<-outtra
    locs$CollarSerialNumber<-as.character(as.numeric(as.character(locs$CollarSerialNumber)))
    #locs<-locs[which(!(locs$CollarSerialNumber %in% mlist)),]
    
    uni<-unique(locs$CollarSerialNumber)
    
    tl<-data.frame()
    for(i in 1:length(uni)){
      vsub<-locs[which(locs$CollarSerialNumber==uni[i]),]
      vsub<-vsub[which(vsub$TelemDate==max(vsub$TelemDate,na.rm=T)),]
      tl<-rbind(tl,vsub)
    }
    
    tl$CollarSerialNumber<-as.numeric(tl$CollarSerialNumber)
    tl<-tl[,c(1,2,6,7)]
    
    ntab<-merge(ntab,tl,by.x='Serial.Number',by.y='CollarSerialNumber')
    
    # if(spp=='deer'){
    if(!('AID' %in% names(ntab))){
      colnames(ntab)[12:14]<-c('LatestTelemDate','LatestEasting','LatestNorthing')
    }
    if('AID' %in% names(ntab)){
      colnames(ntab)[13:15]<-c('LatestTelemDate','LatestEasting','LatestNorthing')
    }
    # }else{
    #   colnames(ntab)[11:13]<-c('LatestTelemDate','LatestEasting','LatestNorthing')
    # }
    
    ntab$CurrentStatus<-ifelse(ntab$CurrentStatus == 'not expelled','Birth Not yet triggered',ntab$CurrentStatus)
    nt1<-ntab
    nt2<-ntab
    
    sp::coordinates(nt1)<-~EventEasting+EventNorthing
    sp::proj4string(nt1)<-"+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
    
    sp::coordinates(nt2)<-~LatestEasting+LatestNorthing
    sp::proj4string(nt2)<-"+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
    
    
    
    #library(rgeos)
    dists<-vector()
    for(p in 1:length(nt1)){
      j<-nt1[p,]
      k<-nt2[p,]
      
      dist<-rgeos::gDistance(j,k)
      dists<-c(dists,dist)
    }
    
    ntab$DistanceFromPreviousEvent<-dists
  }
  
  
  if(spp=='elk'){
    vi<-vi[which(as.character(as.numeric(vi$CollarSerialNumber)) %in% vhist$Serial.Number),]
    cursas<-vi
    cs<-data.frame()
    uni<-unique(outtra$CollarSerialNumber)
    for(i in 1:length(uni)){
      s<-outtra[which(outtra$CollarSerialNumber==uni[i]),]  
      
      s<-s[order(s$TelemDate),]
      s<-s[nrow(s),]
      cs<-rbind(cs,s)
    }
    
    cs<-cs[,c(1,2,6,7)]
    # if(spp=='deer'){
    #   ntab<-vhist[,c(1,8,9,3,4,5,2)]
    # }else{
    ntab<-vhist
    
    cs$CollarSerialNumber<-as.numeric(cs$CollarSerialNumber)
    
    ntab<-merge(ntab,cs,by.x='Serial.Number',by.y='CollarSerialNumber',all.x=T)

}
  return(ntab)
}