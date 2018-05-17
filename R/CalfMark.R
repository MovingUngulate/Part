#' @title Daily Calf Update
#
#' @description Run daily parturition calf updates data manipulation and plotting
#' @param vecpath path to vectronic data folder
#' @param ATSUsers character vector of ATS user names
#' @param ATSPass character vector of ATS passwords
#' @param tempdir temporary folder for downloading data
#' @param spp Species of animal
#' @param ncpu number of CPU cores for multithreaded work
#' @param lookup path to lookup table
#' @return Creates all data needed for fawnmark
#' @keywords fawnmark, prep
#' @export
#' @examples
#' \donttest{vecVit<-vecVitDat(path='F:/Box Sync/DEER/Data/Vectronic/VecData')}
#'

CalfMark<-function(ATSUser,ATSPass,tempdir,
                   ncpu,lookup,from=NA,to=NA,subject=NA,SP=NA,
                   progpath=NA,username=NA,password=NA,email='no',systype='Win'){
  
  
  
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
  
  
  dd<-Part::ColDownload(username = ATSUser,password=ATSPass,
                        dirdown = tempdir)
  dat<-dd[[1]]
  
  uni<-unique(dat$CollarSerialNumber)
  
  lastpoint<-data.frame()
  for(i in 1:length(uni)){
    sub<-dat[dat$CollarSerialNumber==uni[i],]
    sub<-sub[order(sub$TelemDate,decreasing = T),]
    sub<-as.data.frame(sub)
    
    lastpoint<-rbind(lastpoint,sub[1,])
  }
  
  
  lastpoint$CollarSerialNumber<-as.character(lastpoint$CollarSerialNumber)
  
  
  vhist<-read.csv(lookup,stringsAsFactors = F)
  vhist<-vhist[complete.cases(vhist$Serial),]
  vhist<-vhist[which(vhist$Species=='EK'),]
  vhist$Serial<-as.character(vhist$Serial)
  
  
  lastpoint<-merge(lastpoint,vhist,by.x='CollarSerialNumber',by.y='Serial',all.x=T)
  lastpoint<-lastpoint[complete.cases(lastpoint$Frequency),]
  
  sp::coordinates(lastpoint)<-~Longitude+Latitude
  sp::proj4string(lastpoint)<-sp::proj4string(dat)
  
  names(lastpoint)[10]<-'name'
  rgdal::writeOGR(lastpoint["name"], paste(tempdir,'Elk_LatestLocs.kml',sep=''), layer = 'ElkLocs', driver = "KML", 
                  overwrite = T)
  
  #lastpoint<-sp::spTransform(lastpoint,'+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  lastpoint<-lastpoint[,c(10)]
  #names(lastpoint)[1]<-'Frequency'
  rgdal::writeOGR(lastpoint,paste(tempdir,'Elk_LatestLocs.gpx',sep=''),layer='locs',driver='GPX',overwrite_layer=T)
  
  
  vi<-dd[[2]]
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
  dat$X2D.3D<-as.numeric(dat$X2D.3D)
  
  
  Cdat<-Part::cleanFun(dat,filename=CleanRep,spp='Elk')
  
  mdat<-as.data.frame(Cdat[[1]])
  mdat$CollarSerialNumber<-as.character(mdat$CollarSerialNumber)
  
  tim<-paste(strftime(Sys.time(),format='%Y'),'-03-01 00:00:00',sep='')
  mdat<-mdat[which(mdat$TelemDate>=as.POSIXct(tim,'%Y-%m-%d %H:%M:%S',tz='MST')),]
  
  atab<-as.data.frame(table(mdat$CollarSerialNumber))
  atab<-atab[which(atab$Freq>100),]
  
  mdat<-mdat[which(mdat$CollarSerialNumber %in% atab$Var1),]
  
  mdat2<-Part::BGBFun(data=mdat,xname='Easting',yname='Northing',timename='TelemDate',
                      idname='CollarSerialNumber',projstring=sp::proj4string(Cdat[[1]]),ncpus=ncpu)
  
  hg<-Part::ElkRFPred(mdat2)
  viout<-Part::locFun(vidat=vi,locdat=mdat2)
  
  viout<-viout[which(viout$CollarSerialNumber %in% mdat2$CollarSerialNumber),]
  
  vhist<-read.csv(lookup,stringsAsFactors = F)
  vhist<-vhist[complete.cases(vhist$Serial),]
  vhist<-vhist[which(vhist$Species=='EK'),]
  #vhist<-vhist[which(!(is.na(vhist$VIT.Freq))),]
  
  #es<-Sys.time()
  
  vi$Date<-as.POSIXct(vi$Date,format='%m/%d/%Y %I:%M:%S %p')
  
  # names(vi)[12]<-'Event'
  names(vhist)[4]<-'Serial.Number'
  
  #mtime<-as.POSIXct('2017-04-01 00:00:00',format='%Y-%m-%d %H:%M:%S')
  tim<-paste(strftime(Sys.time(),format='%Y'),'-04-01 00:00:00',sep='')
  vi<-vi[which(vi$Date>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
  
  
  Part::vitMap(locdat=mdat2,vidat=vi,vhist=vhist,fold=plotfolder,
               spp='elk',plotdataPath=plotdatapath,hg=hg)
  
  
  
  #This function creates a table with updated stats for all animals
  tabby<-Part::tabFun(vhist=vhist,vi=vi,viout=viout,outtra=mdat2,spp='elk')
  
  tabby<-tabby[,c(1,4,2,3,6,7,8)]
  
  colnames(tabby)<-c('Serial','Mom Freq','#Fetus','VIT Freq',
                     'LatestTelemdate','LatestEasting','LatestNorthing')
  
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
                   pathloc=paste0(tempdir,'path.RDS'),plotpath=llist[i],
                   chks=paste0(tempdir,'PlotData_Elk.RDS'),stringsAsFactors=F)
    #saveRDS(fn,paste0(tempdir,'AllPaths.RDS'))
    #saveRDS(llist[i],paste0(paste0(pdfpath,'rds/'),'path','.RDS'))
    
    sb<-gsub(plotfolder,'',llist[i])
    sb<-gsub('.png','',sb)
    if(i == 1){
      rmarkdown::render(input=system.file("rmd/BaseCalfMark.Rmd", package="Part"), 
                        output_format = 'pdf_document',
                        output_file=paste0(pdffolder,'/1000','.pdf'),
                        params=list(tabby=fn[,1],chks=fn[,5]),quiet=T)
      
    }
    if((i+1)<10){
      of<-paste0(paste0(pdffolder,'/100'),i+1,'.pdf')
    }
    if((i+1)>=10){
      of<-paste0(paste0(pdffolder,'/10'),i+1,'.pdf')
    }
    if((i+1)>=100){
      of<-paste0(paste0(pdffolder,'/1'),i+1,'.pdf')
    }
    rmarkdown::render(input=system.file("rmd/PartPlots.Rmd", package="Part"), 
                      output_format = 'pdf_document',
                      output_file=of,
                      params=list(tabby=fn[,1],
                                  ll=fn[,2],
                                  plotlink=fn[,4],
                                  basepath=paste0(plotfolder,'/')),quiet=T)
    
  }
  
  endtime<-Sys.time()
  
  c<-list.files(pdffolder,full.names=T)
  c<-c('pdftk',c,'output',paste0(tempdir,'CalfMark.pdf'))
  
  c<-paste(c,collapse=' ')
  system(c)
  
  if(systype=='linux'){
    c<-c(paste('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/ebook -dNOPAUSE -dBATCH  -dQUIET -sOutputFile=',paste0(tempdir,'FawnMark.pdf'),paste0(tempdir,'FawnMarkCom.pdf')),sep=' ')
    system(c)
    file.remove(paste0(tempdir,'CalfMark.pdf'))
    file.rename(paste0(tempdir,'CalfMarkCom.pdf'),paste0(tempdir,'CalfMark.pdf'))
    
    
  }
  
  if(email=='yes'){
    attt<-c(paste0(tempdir,'Elk_LatestLocs.kml'),
            paste0(tempdir,'Elk_LatestLocs.gpx'), 
            paste0(tempdir,'CalfMark.pdf'))
    
    Part::sendUpdate(from=from,to=to,
               subject=subject,SP=SP,
               attachpath=attt,
               progpath=progpath,
               username=username,password=password,systype)
  }
  
  return(paste0('All Done! Took ',round(abs(as.numeric(difftime(endtime,begtime,units='mins'))),digits=1),
                ' minutes to run!'))
  
}