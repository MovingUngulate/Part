#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param dat cleaned movement data as data.frame
#' @param ncpus proj4string of dat projection
#' @param folder character vector shortname of dataset (eg. 'SMElk')
#' @param mean_date Either 'Animal' or 'Study'. Most should use 'Study'
#' @return List of length 2. 1) aggregates the model training - prediction results, 2) aggregates the predicted results to make a final prediction. Also saves all steps in /Results folder within the folder given to function
#' @keywords part, parturition, parturition prediction
#' @export
Part_PredWrap<-function(rfmod,dat,mean_date,idname='UAID',timename='time',projstring,
                      dataset,time.zone,ncpus,
                      folder,type='NPred',imp=FALSE){

  ab<-Sys.time()

  data<-Part::Part_calcmoveStats(dat = dat, projstring = projstring,
                           dataset = dataset, time.zone = time.zone, ncpus = ncpus,
                           folder=folder,type=type,imp=imp)
  cat('Finished with MoveStats:',format(Sys.time(),'%m/%d/%Y %H:%M'))
  data$doy<-as.numeric(strftime(data$time,format='%j'))

  data<- data[which(data$doy>=mean_date-30&data$doy<=mean_date+30),]
  if(dir.exists(folder)==FALSE){
    dir.create(folder)
  }
  if(dir.exists(paste0(folder,'/NewPreds'))==FALSE){
    dir.create(paste0(folder,'/NewPreds'))
  }
  saveRDS(data, file=paste0(folder,'/NewPreds/PredictionReady_',gsub('-','',Sys.Date()),'.RDS'))

  cl<-snow::makeSOCKcluster(rep('localhost',ncpus))
  snow::clusterEvalQ(cl,c(library(randomForest),library(dplyr)))
  # uses each RF to predict out to all animals
  nstrain<-snow::parLapply(cl,rfmod,fun=Part::Part_Predict, data=data,
                     idname=idname,timename=timename)
  snow::stopCluster(cl)
  saveRDS(nstrain, file=paste0(folder,'/NewPreds/PredictionInterm_',gsub('-','',Sys.Date()),'.RDS'))
  cat("\n")
  cat('Finished with Predictions:',format(Sys.time(),'%m/%d/%Y %H:%M'))
  #saveRDS(nstrain, file='C:/Users/mhayes1/Desktop/CAElk_nPred.RDS')
  # pulling out raw data results
  #Mean value is Actual minus Predicted
  #negative values are predicting late, positive are predicting early

  nres<-Part::Part_npredResults(nstrain) # this one aggregates the predicted results to make a final prediction

  saveRDS(nres, file=paste0(folder,'/NewPreds/FinalNewPredictions_',gsub('-','',Sys.Date()),'.RDS'))
  cat("\n\n\n")
  cat('Predictions Ran in', round(difftime(Sys.time(),ab,units='mins'),digits=3),'minutes')
  # save NRES as final_results
  #saveRDS(nres, file="/Users/efuller/Google Drive/Parturition/FINAL/02202016_NewElk_BFHElk_FinalOutputs.RDS")
  return(nres)
}
