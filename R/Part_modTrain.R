#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param nrun number of bootstraps to run #vector('list',200)
#' @param data prep[[1]][[1]]
#' @param vars output of varprep
#' @param mt output of mt
#' @param sampsize percent of sample to use, 80 default
#' @param idname column name of unique animal id
#' @param partname column name of birth column
#' @param timename column name of posix time
#' @param partdoyname column name of julian day column
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
Part_modTrain<-function(nrun,data,vars,mt,sampsize,idname='UAID',
                       partname='birth',timename='time',partdoyname='Date.of.Birth'){

  data <- as.data.frame(data)
  #need only complete data
  data<-data[complete.cases(data[,vars]),]

  #normalize covariates
  #data[,vars] <- data.frame(lapply(data[,vars], function(X) (X - min(X))/diff(range(X))))

  #create vector of unique animal ids
  uni<-unique(data[,idname])

  # translate percentage into integer number of ani
  n.animals = floor(length(uni)*(sampsize/100))

  #randomly grab n.animals of UAIDs
  samp<-sample(uni, n.animals)

  #create model training data from sample subset
  traindata<-data[which(data[,idname] %in% samp),]

  #create model validation data from sample subset
  testdata<-data[which(!data[,idname] %in% samp),]

  #make partcode a factor
  traindata[,partname]<-as.factor(as.character(traindata[,partname]))


  rf.train = randomForest::randomForest(x=traindata[,vars],y=traindata[,partname], data = traindata, mtry=mt,ntree=300)


  #run training model

  #predict model to testing data
  predRF<-as.data.frame(predict(rf.train,testdata[,vars],type="prob"))
  respRF<-as.data.frame(predict(rf.train,testdata[,vars],type="response"))

  #make column names that mean something
  colnames(predRF)<-c('RFProb0','RFProb1')
  colnames(respRF)<-c('RFCode')

  #bind the test data together
  testdata<-cbind(testdata,predRF,respRF)
  testdata<-testdata[,c(idname,timename,partname,partdoyname,'RFProb0','RFProb1','RFCode')]

  #ensure that time is in POSIX
  testdata$time<-as.POSIXct(as.character(testdata$time),format='%Y-%m-%d %H:%M:%S')
  #create a DOY column
  testdata$DOY<-as.numeric(strftime(testdata$time, format='%j'))

  testdata$PartDOY<-as.numeric(strftime(testdata[,partdoyname],format='%j'))

  # build dataframe of results
  # individual, actual birthday, predicted birthday, and differences between each

  uni<-unique(testdata[,idname])
  tm<-data.frame()

  # loop through each of the cutoff values
  # for(p in 1:length(cutlist)){

  #

  for(f in 1:length(uni)){
    # loop through each individaul
    subd<-testdata[which(testdata[,idname] == uni[f]),]

    # make dataframe of results for individual
    outty<-data.frame(UAID = uni[f],
                      Actual.DOB=unique(subd$PartDOY),
                      DOB_RFProb = min(subd$DOY[which.max(subd$RFProb1)]),
                      MaxRFVal = max(subd$RFProb1,na.rm=T),
                      MeanRFVal = mean(subd$RFProb1,na.rm=T),
                      MedianRFVal = median(subd$RFProb1,na.rm=T),
                      LowQuant = quantile(subd$RFProb1,probs=seq(0,1,0.1),na.rm=T)[2],
                      UpQuant = quantile(subd$RFProb1,probs=seq(0,1,0.1),na.rm=T)[10],
                      stringsAsFactors = FALSE
    )

    # negative values are after parturturition, positive are before
    outty$RFProbDif <- outty$Actual.DOB - outty$DOB_RFProb


    tm<-rbind(tm,outty)
  }

  return(list(rf.train,tm))

}
