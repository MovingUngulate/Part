#' @title Conversion of neonate data into the proper format for conducting survival analysis 
#
#' @description Convert capture data of neonates into the proper format for conducting survival analysis. This function will return 2 columns of data: survival time of each in 
#' whatever units desired (days, weeks, months, etc.) and an indicator column of whether the event (i.e. mortality) occurred in binary format. Further, this function allows the option to 
#' use a cutoff age for modeling survival. For instance, in many cases survival of neonatal ungulates is modeled to 140 days. 
#' @param NeoDat data.frame of capture data. Should contain a column of the day an animal began being monitored (must be named StartDate and in Date format), a column of animal mortality date or
#' or current date if individual still alive (must be named EndDate and in Date format), and a column identifying cause of mortality. 
#' @param units Units desired for modeling survival (e.g. days, weeks, etc)
#' @param cuts = Logical. TRUE/FALSE. Whether you desire to use a cutoff date for survival
#' @param cutoffs Vector of cutoffs in units desired (e.g. 140 days, indicate 140)
#' @param Cause Vector of causes in your data.frame in which events should be censored (i.e. Collar_Failure)
#' @return Returns a data.frame with original data.frame with a column of survival time (DiffTime) and indicator of whether event occurred (Indicator)
#' @keywords neonate, survival
#' @export
#' @examples
#' \donttest{NeoSurv<-NeoSurvDat(NeoDat = yourdata, units = "days", cuts = TRUE, cutoffs = 140, Cause = "collar_failure")}
#' 
NeoSurvData<-function(NeoDat, units, cuts, cutoffs, Cause){
NeoDat<-NeoDat[complete.cases(NeoDat$StartDate),]
NeoDat<-NeoDat[complete.cases(NeoDat$EndDate),]
NeoDat$DiffTime<-as.numeric(difftime(NeoDat$EndDate, NeoDat$StartDate, units = units))
NeoDat$Year<-strftime(NeoDat$StartDate, format = "%Y")

if(cuts == TRUE){
  uni<-unique(NeoDat$Year)
  x<-data.frame()
  for(l in 1:length(uni)){
    sub<-NeoDat[NeoDat$Year == uni[l],]
    sub$Cut<-ifelse(sub$DiffTime > cutoffs, 0, 1)
    
    x<-rbind(sub, x)
  }
  x$Event<-ifelse(x$Cause %in% Cause, 0, 1)
  x$Indicator<-ifelse(x$Cut == 1 & x$Event == 1, 1, 0)
  return(x)
}

if(cuts == FALSE){
  return(NeoDat)
}
}

