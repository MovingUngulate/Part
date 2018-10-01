#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param x output of nPred
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
Part_npredResults<-function(x){
  boots<-as.data.frame(data.table::rbindlist(x))

  bagg<-aggregate(boots$DOB_RFProb,by=list(boots$UAID),FUN=mean,na.rm=T)

  bagg$MedianDif<-aggregate(boots$DOB_RFProb,by=list(boots$UAID),FUN=median,na.rm=T)[,2]
  bagg$SD<-aggregate(boots$DOB_RFProb,by=list(boots$UAID),FUN=sd,na.rm=T)[,2]
  colnames(bagg)<-c('UAID','MeanDOY','MedianDOY','SDDOY')
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
