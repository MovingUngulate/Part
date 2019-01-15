#' @title Rarefy Parturition Model
#
#' @description This function tests the effects of rarefying your parturition model on model accuracy. You
#' can use this to help determine the fix rate needed for you data/study/species
#' @param input Username for Iridium Account (not stored) as character vector
#' @param name Password for Iridium Account (not stored) as character vector
#' @param saveby Directory to download the file to
#' @param ncpu Type of collar/system (currently only supports 'ATS/IRID' and 'ATS/GSTAR')
#' @param basefolder folder to save output to (eg '/home/puma/Desktop/Parts/')
#' @param sampsize Percent of animals to use to train model (default percent = 80)
#' @param prepboots bootstraps to use when preparing variables/model (~20 is sufficient)
#' @param finboots bootstraps to use when creating final model (200-1000)
#' @param max_fix max fix rate to test expressed as number (ie 12)
#' @return outputs a new folder for each fix rate, goes from 1 to max_fix
#'
#' @keywords parturition fix rate, fixrate
#' @export
#' @examples
#' \donttest{fixrate(input = "/home/puma/Desktop/RunningPart/Data/StarkeyElk_ReadyData.RData", name = 'StarkElk', saveby = 'Study', ncpu = 75, basefolder = '/home/puma/Desktop/RunningPart/NewRarefy/', sampsize = 80, prepboots = 75, finboots = 100, max_fix = 12)}
#'


Part_fixrate<-function(input="/home/puma/Desktop/RunningPart/Data/StarkeyElk_ReadyData.RData", 
                  name, 
                  saveby = 'Study', 
                  ncpu = 75, 
                  basefolder, 
                  sampsize = 80, 
                  prepboots = 75, 
                  finboots = 100,
                  max_fix = 12){
  
  for(i in 1:max_fix){
    
    load(input)
    if(exists('bday_dat')){
      bd<-bday_dat
    }
    bd$DOY<-as.numeric(strftime(bd$Date.of.Birth,'%j'))
    
    uni<-unique(dat$UAID)
    uni<-uni[complete.cases(uni)]
    
    d2<-data.frame()
    for(k in 1:length(uni)){
      sub<-dat[dat$UAID==uni[k],]
      
      sub<-sub[order(sub$TelemDate),]
      
      sub<-sub[seq(1,nrow(sub),i),]
      
      d2<-rbind(d2,sub)
    }
    
    dat<-d2
    
    tab<-as.data.frame(table(dat$UAID))
    tab<-tab[tab$Freq>50,]
    
    dat<-dat[dat$UAID %in% tab$Var1,]
    
    parts<-Part::Part_Wrap(dat=dat,projstring=proj,time.zone=time.zone,dataset=name,
                           saveby=saveby,ncpus=ncpu,folder=paste0(basefolder,name,'_',i),
                           mean_date=mean(bd$DOY),bday_dat=bd,idname='UAID',sampsize=sampsize,prepBoots=prepboots,finBoots=finboots)
    
    save.image(paste0(basefolder,name,"_rarefy_",i,".RData"))
  }
} 