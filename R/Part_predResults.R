#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param x output of modTrain
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
Part_predResults<-function(x){

  boots = data.frame()
  for(j in 1:length(x)){
    boots = rbind(boots, x[[j]][[2]])
  }

  bagg<-aggregate(boots$RFProbDif,by=list(boots$UAID),FUN=mean,na.rm=T)

  bagg$MedianDif<-aggregate(boots$RFProbDif,by=list(boots$UAID),FUN=median,na.rm=T)[,2]
  bagg$SD<-aggregate(boots$RFProbDif,by=list(boots$UAID),FUN=sd,na.rm=T)[,2]
  colnames(bagg)<-c('UAID','MeanDif','MedianDif','SD')
  bagg$MeanDOY<-aggregate(boots$DOB_RFProb,by=list(boots$UAID),FUN=mean,na.rm=T)[,2]

  bagg$MedianDOY<-aggregate(boots$DOB_RFProb,by=list(boots$UAID),FUN=median,na.rm=T)[,2]
  bagg$SDDOY<-aggregate(boots$DOB_RFProb,by=list(boots$UAID),FUN=sd,na.rm=T)[,2]
  bagg$MaxRFValMean<-aggregate(boots$MaxRFVal,by=list(boots$UAID),FUN=median,na.rm=T)[,2]
  bagg$MaxRFValSD<-aggregate(boots$MaxRFVal,by=list(boots$UAID),FUN=sd,na.rm=T)[,2]
  bagg$MeanRFValMean<-aggregate(boots$MeanRF,by=list(boots$UAID),FUN=median,na.rm=T)[,2]
  bagg$MeanRFValSD<-aggregate(boots$MeanRF,by=list(boots$UAID),FUN=sd,na.rm=T)[,2]
  bagg$MedianRFValMean<-aggregate(boots$MedianRFVal,by=list(boots$UAID),FUN=median,na.rm=T)[,2]
  bagg$MedianRFValSD<-aggregate(boots$MedianRFVal,by=list(boots$UAID),FUN=sd,na.rm=T)[,2]
  bagg$LowQuantMean<-aggregate(boots$LowQuant,by=list(boots$UAID),FUN=median,na.rm=T)[,2]
  bagg$LowQuantSD<-aggregate(boots$LowQuant,by=list(boots$UAID),FUN=sd,na.rm=T)[,2]
  bagg$UpQuantMean<-aggregate(boots$UpQuant,by=list(boots$UAID),FUN=median,na.rm=T)[,2]
  bagg$UpQuantSD<-aggregate(boots$UpQuant,by=list(boots$UAID),FUN=sd,na.rm=T)[,2]


  return(bagg)
}
