#' @title Daily Fawn Update
#
#' @description Run daily parturition fawn updates data manipulation and plotting
#' @param vecpath path to vectronic data folder
#' @param ATSUsers character vector of ATS user names
#' @param ATSPass character vector of ATS passwords
#' @param tempdir temporary folder for downloading data
#' @param spp Species of animal
#' @param ncpu number of CPU cores for multithreaded work
#' @param lookup path to lookup table
#' @param ST do you have SirTrack data? TRUE/FALSE
#' @param STUser SirTrack Username
#' @param STPass SirTrack Password
#' @return Creates all data needed for fawnmark
#' @keywords fawnmark, prep
#' @export
#' @examples
#' \donttest{vecVit<-vecVitDat(path='F:/Box Sync/DEER/Data/Vectronic/VecData')}
#'

FawnMark<-function(vecpath,ATSUsers,ATSPass,tempdir,
                       ncpu,lookup,from=NA,to=NA,subject=NA,SP=NA,
                       progpath=NA,username=NA,password=NA,email='no',ST=TRUE,
                       STUser=NULL,STPass=NULL,systype='Win'){
  
  spp<-'FMD'
  options(warn=-1)
  if(dir.exists(tempdir) == TRUE){
    llist<-list.files(tempdir,all.files=T,full.names=T)
    for(i in 3:length(llist)){
      file.remove(llist[i])
    }
    CleanRep<-paste0(tempdir,'CleaningReport.txt')
    datastore<-paste0(tempdir,'datastore.RDS')
    PrettyDataStore<-paste0(tempdir,'PrettyData.RDS')
    plotfolder<-paste0(tempdir,'plots')
    dir.create(plotfolder)
    pdffolder<-paste0(tempdir,'PDFs')
    dir.create(pdffolder)
    plotdatapath<-paste0(tempdir,'PlotData')
    #dir.create(plotdatapath)
    
  }
  if(dir.exists(tempdir) == FALSE){
    dir.create(tempdir)
    CleanRep<-paste0(tempdir,'CleaningReport.txt')
    datastore<-paste0(tempdir,'datastore.RDS')
    PrettyDataStore<-paste0(tempdir,'PrettyData.RDS')
    plotfolder<-paste0(tempdir,'plots')
    dir.create(plotfolder)
    pdffolder<-paste0(tempdir,'PDFs')
    dir.create(pdffolder)
    plotdatapath<-paste0(tempdir,'PlotData')
    #dir.create(plotdatapath)
  }
  options(warn=0)
  
  
  llist<-list.files(plotfolder,full.names=T)
  if(length(llist)>0){
    for(i in 1:length(llist)){
      file.remove(llist[i])
    }
  }
  llist<-list.files(pdffolder,full.names=T)
  if(length(llist)>0){
    for(i in 1:length(llist)){
      file.remove(llist[i])
    }
  }
  
  
  begtime<-Sys.time()
  
  dat<-Part::CombDat(vecpath=vecpath,
               ATSUsers=ATSUsers,ATSPass=ATSPass,
               tempdir=tempdir,ST,STUser,STPass)
  
  vi<-readRDS(paste0(tempdir,'DDown.RDS'))
  dat2<-dat
  
  uni<-unique(dat2$CollarSerialNumber)
  
  lastpoint<-data.frame()
  for(i in 1:length(uni)){
    sub<-dat2[dat2$CollarSerialNumber==uni[i],]
    sub<-sub[order(sub$TelemDate,decreasing = T),]
    sub<-as.data.frame(sub)
    
    lastpoint<-rbind(lastpoint,sub[1,])
  }
  
  
  lastpoint$CollarSerialNumber<-as.character(lastpoint$CollarSerialNumber)
  
  
  vhist<-read.csv(lookup,stringsAsFactors = F)
  vhist<-vhist[complete.cases(vhist$Serial),]
  vhist<-vhist[which(vhist$Species=='FMD'),]
  vhist$Serial<-as.character(vhist$Serial)
  
  
  lastpoint<-merge(lastpoint,vhist,by.x='CollarSerialNumber',by.y='Serial',all.x=T)
  lastpoint<-lastpoint[complete.cases(lastpoint$Frequency),]
  
  sp::coordinates(lastpoint)<-~Longitude+Latitude
  sp::proj4string(lastpoint)<-sp::proj4string(dat)
  
  names(lastpoint)[8]<-'name'
  rgdal::writeOGR(lastpoint["name"], paste(tempdir,'Deer_LatestLocs.kml',sep=''), layer = 'ElkLocs', driver = "KML", 
                  overwrite = T)
  
  #lastpoint<-sp::spTransform(lastpoint,'+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  lastpoint<-lastpoint[,c(8)]
  #names(lastpoint)[1]<-'Frequency'
  rgdal::writeOGR(lastpoint,paste(tempdir,'Deer_LatestLocs.gpx',sep=''),layer='locs',driver='GPX',overwrite_layer=T)
  
  
  
  
  
  
  
  names(vi)[1]<-'CollarSerialNumber'
  names(vi)[2]<-'Date'
  vi$Da<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Date'],' '))[[1]]})
  vi$Ti<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Date'],' '))[[2]]})
  vi$PM<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Date'],' '))[[3]]})
  
  vi$Month<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Da'],'/'))[[1]]})
  vi$Day<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Da'],'/'))[[2]]})
  vi$Year<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Da'],'/'))[[3]]})
  
  vi$Ti<-ifelse(nchar(vi$Ti)==7,paste0('0',vi$Ti),vi$Ti)
  vi$Month<-ifelse(nchar(vi$Month)==1,paste0('0',vi$Month),vi$Month)
  vi$Day<-ifelse(nchar(vi$Day)==1,paste0('0',vi$Day),vi$Day)
  
  vi$Da<-paste(vi$Month,vi$Day,vi$Year,sep='/')
  
  vi$Date<-paste(vi$Da,vi$Ti, vi$PM,sep=' ')
  dat$X2D.3D<-ifelse(dat$X2D.3D=='val. GPS-3D',6,dat$X2D.3D)
  dat$X2D.3D<-ifelse(dat$X2D.3D=='GPS-3D',6,dat$X2D.3D)
  dat$X2D.3D<-ifelse(dat$X2D.3D=='GPS-2D',6,dat$X2D.3D)
  dat$X2D.3D<-as.numeric(dat$X2D.3D)
  
  
  Cdat<-Part::cleanFun(dat,filename=CleanRep,spp=spp)
  
  mdat<-as.data.frame(Cdat[[1]])
  mdat$CollarSerialNumber<-as.character(mdat$CollarSerialNumber)
  
  tim<-paste(strftime(Sys.time(),format='%Y'),'-03-01 00:00:00',sep='')
  mdat<-mdat[which(mdat$TelemDate>=as.POSIXct(tim,'%Y-%m-%d %H:%M:%S',tz='MST')),]
  
  atab<-as.data.frame(table(mdat$CollarSerialNumber))
  atab<-atab[which(atab$Freq>100),]
  
  mdat<-mdat[which(mdat$CollarSerialNumber %in% atab$Var1),]
  
  mdat<-mdat[as.character(as.numeric(mdat$CollarSerialNumber)) %in% vhist$Serial,]
  
  mdat2<-Part::BGBFun(data=mdat,xname='Easting',yname='Northing',timename='TelemDate',
                idname='CollarSerialNumber',projstring=sp::proj4string(Cdat[[1]]),ncpus=ncpu)
  
  if(nchar(vecpath)>0){
    vecVit<-Part::vecVitDat(path=vecpath)
    vecVit$Date<-as.POSIXct(vecVit$Date,format='%m/%d/%Y %H:%M:%S %p')
    vi$Date<-as.POSIXct(vi$Date,format='%m/%d/%Y %I:%M:%S %p')
    vi<-rbind(vi,vecVit)
  }
  if(nchar(vecpath)==0){
    vi$Date<-as.POSIXct(vi$Date,format='%m/%d/%Y %I:%M:%S %p')
  }
  
  vi$`NeoLink Status`<-ifelse(vi$`NeoLink Status` == 'not expelled','Birth Not yet triggered',vi$`NeoLink Status`)
  vi$`NeoLink Status`<-ifelse(vi$`NeoLink Status` == 'expelled','Birth-triggered by light and temperature',vi$`NeoLink Status`)
  vi$`NeoLink Status`<-ifelse(vi$`NeoLink Status` == 'Unknown/No Sync','No Status',vi$`NeoLink Status`)
  vi$`NeoLink Status`<-ifelse(vi$`NeoLink Status` == 'Unknown Status Message','No Status',vi$`NeoLink Status`)
  
  if('AID' %in% names(vhist)){
    ddd<-vi[1,]
    td<-Sys.time()
    nf=18
    BattVoltage=7
    Mortality='No'
    BreakOff='No'
    GPSOnTime='1234'
    SatOnTime='1234'
    SatErrors='3'
    GMTOffset='7'
    LowBatt='No'
    Event='No Status'
    Da=strftime(td,'%m/%d/%Y')
    Ti=strftime(td,'%H:%M:%S')
    PM=strftime(td,'%p')
    Month=strftime(td,'%m')
    Day=strftime(td,'%d')
    Year=strftime(td,'%Y')
    
    vv<-c('63834830','63837290','63920580','63938740')
    
    jhg<-data.frame()
    for(i in 1:length(vv)){
      #stl<-ddd
      ddd$CollarSerialNumber<-vv[i]
      ddd$Date<-td
      ddd$NumFixes<-nf
      ddd$BattVoltage<-BattVoltage
      ddd$Mortality<-Mortality
      ddd$BreakOff<-BreakOff
      ddd$GPSOnTime<-GPSOnTime
      ddd$SatOnTime<-SatOnTime
      ddd$SatErrors<-SatErrors
      ddd$GMTOffset<-GMTOffset
      ddd$LowBatt<-LowBatt
      ddd$`NeoLink Status`<-Event
      ddd$Da<-Da
      ddd$Ti<-Ti
      ddd$PM<-PM
      ddd$Month<-Month
      ddd$Day<-Day
      ddd$Year<-Year
      jhg<-rbind(jhg,ddd)
    }
    vi<-rbind(vi,jhg)
    
  }
  viout<-Part::locFun(vidat=vi,locdat=mdat2)
  
  viout<-viout[which(viout$CollarSerialNumber %in% mdat2$CollarSerialNumber),]
  
  vhist<-read.csv(lookup,stringsAsFactors = F)
  vhist<-vhist[complete.cases(vhist$Serial),]
  vhist<-vhist[which(vhist$Species==spp),]
  #vhist<-vhist[which(!(is.na(vhist$VIT.Freq))),]
  
  #es<-Sys.time()
  
  #vi$Date<-as.POSIXct(vi$Date,format='%m/%d/%Y %I:%M:%S %p')
  
  names(vi)[12]<-'Event'
  names(vhist)[4]<-'Serial.Number'
  
  #mtime<-as.POSIXct('2017-04-01 00:00:00',format='%Y-%m-%d %H:%M:%S')
  tim<-paste(strftime(Sys.time(),format='%Y'),'-03-01 00:00:00',sep='')
  vi<-vi[which(vi$Date>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
  

  
  Part::vitMap(locdat=mdat2,vidat=vi,vhist=vhist,fold=plotfolder,
                       spp='deer',plotdataPath=plotdatapath)
  
  
  
  #This function creates a table with updated stats for all animals
  tabby<-Part::tabFun(vhist=vhist,vi=vi,viout=viout,outtra=mdat2,spp='deer')
  
  if(!('AID' %in% names(tabby))){
    tabby<-tabby[,c(1,4,2,3,7,8,9,10,11,12:15)]
    
    colnames(tabby)<-c('Serial','Mom Freq','#Fetus','VIT Freq','VitStatusChange',
                       'VitStatusChange_3Day','EventEasting','EventNorthing','CurrentVitStatus',
                       'LatestTelemdate','LatestEasting','LatestNorthing','DistFromEvent')
  }
  if('AID' %in% names(tabby)){
    tabby<-tabby[,c(1,4,2,3,8,9,10,11,12,13:16,6)]
    
    colnames(tabby)<-c('Serial','Mom Freq','#Fetus','VIT Freq','VitStatusChange',
                       'VitStatusChange_3Day','EventEasting','EventNorthing','CurrentVitStatus',
                       'LatestTelemdate','LatestEasting','LatestNorthing','DistFromEvent','AID') 
  }
  saveRDS(tabby,file=datastore)
  
  
  Part::PrettyData(dat=mdat2,idl=tabby,filen=PrettyDataStore)
  
  fn<-data.frame(datastore=datastore,prettydatastore=PrettyDataStore,
                 pathloc=paste0(tempdir,'path.RDS'),stringsAsFactors=F)
  saveRDS(fn,paste0(tempdir,'AllPaths.RDS'))
  
  
  path<-plotfolder
  # pdfpath<-'C:/Users/mhayes1/Desktop/FreshStart/PDFs/'
  llist<-list.files(path,full.names=T)
  for(i in 1:length(llist)){
    
    #sb<-gsub('C:/Users/mhayes1/Desktop/FreshStart/plots/','',llist[i])
    #sb<-gsub('.png','',sb)
    
    fn<-data.frame(datastore=datastore,prettydatastore=PrettyDataStore,
                   pathloc=paste0(tempdir,'path.RDS'),plotpath=llist[i],stringsAsFactors=F)
    #saveRDS(fn,paste0(tempdir,'AllPaths.RDS'))
    #saveRDS(llist[i],paste0(paste0(pdfpath,'rds/'),'path','.RDS'))
    
    sb<-gsub(plotfolder,'',llist[i])
    sb<-gsub('.png','',sb)
    if(i == 1){
      rmarkdown::render(input=system.file("rmd/BaseMark.Rmd", package="Part"), 
                        output_format = 'pdf_document',
                        output_file=paste0(pdffolder,'/1000','.pdf'),
                        params=list(tabby=fn[,1]),quiet=T)
      
    }
    rmarkdown::render(input=system.file("rmd/PartPlots.Rmd", package="Part"), 
                      output_format = 'pdf_document',
                      output_file=paste0(paste0(pdffolder,'/100'),i+1,'.pdf'),
                      params=list(tabby=fn[,1],
                                  ll=fn[,2],
                                  plotlink=fn[,4],
                                  basepath=paste0(plotfolder,'/')),quiet=T)
    
  }
  
  endtime<-Sys.time()
  
  c<-list.files(pdffolder,full.names=T)
  c<-c('pdftk',c,'output',paste0(tempdir,'FawnMark.pdf'))
  
  c<-paste(c,collapse=' ')
  system(c)

  if(email=='yes'){
  #attt<-paste0(tempdir,'FawnMark.pdf')
  
  attt<-c(paste0(tempdir,'Deer_LatestLocs.kml'),
          paste0(tempdir,'Deer_LatestLocs.gpx'), 
          paste0(tempdir,'FawnMark.pdf'))
  
  Part::sendUpdate(from=from,to=to,
             subject=subject,SP=SP,
             attachpath=attt,
             progpath=progpath,
             username=username,password=password,systype)
  }
  
  return(paste0('All Done! Took ',round(abs(as.numeric(difftime(endtime,begtime,units='mins'))),digits=1),
                ' minutes to run!'))
  
}