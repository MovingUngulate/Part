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
#' @param mortvec character vector of mortalities 
#' @return Creates all data needed for fawnmark
#' @keywords fawnmark, prep
#' @export
#' @examples
#' \donttest{vecVit<-vecVitDat(path='F:/Box Sync/DEER/Data/Vectronic/VecData')}
#'

FawnMark<-function(vecpath,ATSUsers,ATSPass,tempdir,
                       ncpu,lookup,mortvec,from=NA,to=NA,subject=NA,SP=NA,
                       progpath=NA,username=NA,password=NA,email='no'){
  
  spp<-'FMD'
  options(warn=-1)
  if(dir.exists(tempdir) == TRUE){
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
  begtime<-Sys.time()
  
  dat<-CombDat(vecpath=vecpath,
               ATSUsers=ATSUsers,ATSPass=ATSPass,
               tempdir=tempdir)
  dd<-ColDownload(username = ATSUsers,password=ATSPass,
                  dirdown = tempdir)
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
  
  
  Cdat<-cleanFun(dat,filename=CleanRep,spp=spp)
  
  mdat<-as.data.frame(Cdat[[1]])
  mdat$CollarSerialNumber<-as.character(mdat$CollarSerialNumber)
  
  tim<-paste(strftime(Sys.time(),format='%Y'),'-03-01 00:00:00',sep='')
  mdat<-mdat[which(mdat$TelemDate>=as.POSIXct(tim,'%Y-%m-%d %H:%M:%S',tz='MST')),]
  
  atab<-as.data.frame(table(mdat$CollarSerialNumber))
  atab<-atab[which(atab$Freq>100),]
  
  mdat<-mdat[which(mdat$CollarSerialNumber %in% atab$Var1),]
  
  mdat2<-BGBFun(mdat,xname='Easting',yname='Northing',timename='TelemDate',
                idname='CollarSerialNumber',projstring=sp::proj4string(Cdat[[1]]),ncpus=ncpu)
  
  vecVit<-vecVitDat(path=vecpath)
  vi<-rbind(vi,vecVit)
  viout<-locFun(vidat=vi,locdat=mdat2)
  
  viout<-viout[which(viout$CollarSerialNumber %in% mdat2$CollarSerialNumber),]
  
  vhist<-read.csv(lookup,stringsAsFactors = F)
  vhist<-vhist[complete.cases(vhist$Serial),]
  vhist<-vhist[which(vhist$Species==spp),]
  #vhist<-vhist[which(!(is.na(vhist$VIT.Freq))),]
  
  #es<-Sys.time()
  
  vi$Date<-as.POSIXct(vi$Date,format='%m/%d/%Y %I:%M:%S %p')
  
  names(vi)[12]<-'Event'
  names(vhist)[4]<-'Serial.Number'
  
  #mtime<-as.POSIXct('2017-04-01 00:00:00',format='%Y-%m-%d %H:%M:%S')
  tim<-paste(strftime(Sys.time(),format='%Y'),'-04-01 00:00:00',sep='')
  vi<-vi[which(vi$Date>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
  
  
  system.time({ vitMap(locdat=mdat2,vidat=vi,vhist=vhist,fold=plotfolder,
                       spp='deer',plotdataPath=plotdatapath) })
  
  
  mlist<-mortvec
  #This function creates a table with updated stats for all animals
  tabby<-tabFun(vhist=vhist,mlist=mlist,vi=vi,viout=viout,outtra=mdat2,spp='deer')
  
  tabby<-tabby[,c(1,4,2,3,7,8,9,10,11,12:15)]
  
  colnames(tabby)<-c('Serial','Mom Freq','#Fetus','VIT Freq','VitStatusChange',
                     'VitStatusChange_3Day','EventEasting','EventNorthing','CurrentVitStatus',
                     'LatestTelemdate','LatestEasting','LatestNorthing','DistFromEvent')
  
  saveRDS(tabby,file=datastore)
  
  
  PrettyData(dat=mdat2,idl=tabby,filen=PrettyDataStore)
  
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
                        output_file=paste0(pdffolder,'/001','.pdf'),
                        params=list(tabby=fn[,1]),quiet=T)
      
    }
    rmarkdown::render(input=system.file("rmd/PartPlots.Rmd", package="Part"), 
                      output_format = 'pdf_document',
                      output_file=paste0(pdffolder,sb,'.pdf'),
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
  attt<-paste0(tempdir,'FawnMark.pdf')
  
  sendUpdate(from=from,to=to,
             subject=subject,SP=SP,
             attachpath=attt,
             progpath=progpath,
             username=username,password=password)
  }
  
  return(paste0('All Done! Took ',round(abs(as.numeric(difftime(endtime,begtime,units='mins'))),digits=1),
                ' minutes to run!'))
  
}