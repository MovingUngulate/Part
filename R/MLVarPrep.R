#' @title Prepare variables for Machine Learning Parturition prediction
#
#' @description Use Machine Learning to predict parturition
#' @param jk output of BGBFun
#' @return data.frame of probability predictions for 0, 1, 2 (pre birth, neonate <12hrs old, neonate>12hrs&neonate<24hrs old)
#' @keywords prediction
#' @export
#' @examples
#' \donttest{hg<-ElkRFPred(mdat2)}
#'
MLVarPrep<-function(jk){
  uni<-unique(jk$CollarSerialNumber)
  jk<-jk[complete.cases(jk[,c(8,9,10,12,13,14,17,18)]),]
  akl<-data.frame()
  for(i in 1:length(uni)){
    sm<-jk[jk$CollarSerialNumber==uni[i],]
    
    sm<-sm[order(sm$TelemDate),]
    
    sm$rlm6<-NA
    sm$rlm6[6:nrow(sm)]<-zoo::rollmean(sm$dist,align=c('right'),k=6)
    
    sm$psl6<-NA
    sm$psl6[6:nrow(sm)]<-zoo::rollmean(sm$paraSd,align=c('right'),k=6)
    
    sm$osl6<-NA
    sm$osl6[6:nrow(sm)]<-zoo::rollmean(sm$orthSd,align=c('right'),k=6)
    
    sm$rlm12<-NA
    sm$rlm12[12:nrow(sm)]<-zoo::rollmean(sm$dist,align=c('right'),k=12)
    
    sm$psl12<-NA
    sm$psl12[12:nrow(sm)]<-zoo::rollmean(sm$paraSd,align=c('right'),k=12)
    
    sm$osl12<-NA
    sm$osl12[12:nrow(sm)]<-zoo::rollmean(sm$orthSd,align=c('right'),k=12)
    
    sm$rlm18<-NA
    sm$rlm18[18:nrow(sm)]<-zoo::rollmean(sm$dist,align=c('right'),k=18)
    
    sm$psl18<-NA
    sm$psl18[18:nrow(sm)]<-zoo::rollmean(sm$paraSd,align=c('right'),k=18)
    
    sm$osl18<-NA
    sm$osl18[18:nrow(sm)]<-zoo::rollmean(sm$orthSd,align=c('right'),k=18)
    
    #sm$NC<-c(0,cumsum(sm$Used[-1L] != sm$Used[-length(sm$Used)]))
    
    
    
    #smbb<-sm[sm$NC==1,]
    
    # tab<-as.data.frame(table(sm$NC))
    # 
    # tab<-tab[tab$Freq>1,]
    
    akl<-rbind(akl,sm)
  }
  
  
  akl<-akl[complete.cases(akl[,c(19:27)]),]
  
  
  
  
  uni<-unique(akl$CollarSerialNumber)
  ald<-data.frame()
  for(i in 1:length(uni)){
    ss<-akl[akl$CollarSerialNumber==uni[i],]
    ss$FPT50<-adehabitatLT::fpt(adehabitatLT::as.ltraj(ss[,c('Easting','Northing')],ss$TelemDate,ss$CollarSerialNumber),50,units='hours')[[1]]$r1
    ss$FPT150<-adehabitatLT::fpt(adehabitatLT::as.ltraj(ss[,c('Easting','Northing')],ss$TelemDate,ss$CollarSerialNumber),150,units='hours')[[1]]$r1
    ss$FPT300<-adehabitatLT::fpt(adehabitatLT::as.ltraj(ss[,c('Easting','Northing')],ss$TelemDate,ss$CollarSerialNumber),300,units='hours')[[1]]$r1
    
    
    ald<-rbind(ald,ss)
    
  }
  
  
  akl<-ald[complete.cases(ald[,c(17:30)]),]
  
  
  return(akl)
  
}
