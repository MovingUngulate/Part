#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param folder number of bootstraps to run #vector('list',200)
#' @param datatype prep[[1]][[1]]
#' @param rfmod output of varprep
#' @param mean_date output of mt
#' @param idname percent of sample to use, 80 default
#' @param timename column name of unique animal id
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
Part_Predict<-function(rfmod,data,idname='UAID',timename='time'){


  #mean_date = as.numeric(format(as.Date("05-28-2009",format="%m-%d-%Y"),"%j"))



  vars<-row.names(rfmod$importance)
  data <- as.data.frame(data)
  #need only complete data
  data<-data[complete.cases(data[,vars]),]

  #normalize covariates
  data[,vars] <- data.frame(lapply(data[,vars], function(X) (X - min(X))/diff(range(X))))

  #run training model

  #predict model to testing data
  predRF<-as.data.frame(predict(rfmod,data[,vars],type="prob"))
  respRF<-as.data.frame(predict(rfmod,data[,vars],type="response"))

  #make column names that mean something
  colnames(predRF)<-c('RFProb0','RFProb1')
  colnames(respRF)<-c('RFCode')

  #bind the test data together
  data<-cbind(data,predRF,respRF)
  data<-data[,c(idname,timename,'RFProb0','RFProb1','RFCode')]

  #ensure that time is in POSIX
  data$time<-as.POSIXct(as.character(data$time),format='%Y-%m-%d %H:%M:%S')
  #create a DOY column
  data$DOY<-as.numeric(strftime(data$time, format='%j'))

  #data$PartDOY<-as.numeric(strftime(data[,partdoyname],format='%j'))

  # build dataframe of results
  # individual, actual birthday, predicted birthday, and differences between each

  uni<-unique(data[,idname])
  tm<-data.frame()

  # loop through each of the cutoff values
  # for(p in 1:length(cutlist)){

  #

  for(f in 1:length(uni)){
    # loop through each individaul
    subd<-data[which(data[,idname] == uni[f]),]

    # make dataframe of results for individual
    outty<-data.frame(UAID = uni[f],
                      DOB_RFProb = min(subd$DOY[which.max(subd$RFProb1)]),
                      MaxRFVal = max(subd$RFProb1,na.rm=T),
                      MeanRFVal = mean(subd$RFProb1,na.rm=T),
                      MedianRFVal = median(subd$RFProb1,na.rm=T),
                      LowQuant = quantile(subd$RFProb1,probs=seq(0,1,0.1),na.rm=T)[2],
                      UpQuant = quantile(subd$RFProb1,probs=seq(0,1,0.1),na.rm=T)[10],
                      stringsAsFactors = FALSE
    )

    # negative values are after parturturition, positive are before
    #outty$RFProbDif <- outty$Actual.DOB - outty$DOB_RFProb


    tm<-rbind(tm,outty)
  }

  return(tm)

}
