#' @title Elk Parturition Prediction with Machine Learning
#
#' @description Use Machine Learning to predict parturition in elk
#' @param jk output of BGBFun
#' @return data.frame of probability predictions for 0, 1, 2 (pre birth, neonate <48hrs old, neonate>48hrs old)
#' @keywords prediction
#' @export
#' @examples
#' \donttest{hg<-ElkRFPred(mdat2)}
#'
ElkRFPred<-function(jk){
  
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
  data("ElkRealTimeRF2",package='Part')
  
  
  akl$Pred0<-as.numeric(randomForest:::predict.randomForest(rf,akl[,c(8,9,10,12,13,14,17,18,19:27)],type='prob')[,1])
  akl$Pred1<-as.numeric(randomForest:::predict.randomForest(rf,akl[,c(8,9,10,12,13,14,17,18,19:27)],type='prob')[,2])
  akl$Pred2<-as.numeric(randomForest:::predict.randomForest(rf,akl[,c(8,9,10,12,13,14,17,18,19:27)],type='prob')[,3])
  
  return(akl[,c(1,2,28:30)])
}