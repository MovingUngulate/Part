#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param dat cleaned movement data as data.frame
#' @param projstring proj4string of dat projection
#' @param dataset character vector shortname of dataset (eg. 'SMElk')
#' @param saveby Either 'Animal' or 'Study'. Most should use 'Study'
#' @param time.zone posix compliant time.zone character vector ('Etc/GMT-8')
#' @param ncpus number of cpus to use for parallel processing (minimum 2)
#' @param folder directory to store data in. If not created, function will create
#' @param mean_date julian date (as.numeric) of the mean date of parturition for your animal/system.
#' @param bday_dat data.frame with UAID and Date.of.Birth columns only
#' @param idname Column name (character vector) for Unique animal ID column (default = 'UAID')
#' @param sampsize Percent of animals to use to train model (default percent = 80)
#' @param prepBoots bootstraps to use when preparing variables/model (~20 is sufficient)
#' @param finBoots bootstraps to use when creating final model (200-1000)
#' @return List of length 2. 1) aggregates the model training - prediction results, 2) aggregates the predicted results to make a final prediction. Also saves all steps in /Results folder within the folder given to function
#' @keywords part, parturition, parturition prediction
#' @export
PartWrap<-function(dat,projstring,dataset,saveby='Study',time.zone,ncpus,folder,
                   mean_date,bday_dat,idname='UAID',sampsize=80,prepBoots=75,finBoots,imp=FALSE){

  ab<-Sys.time()

  if(dir.exists(folder)==TRUE){
    unlink(folder,recursive = T)
  }
  if(dir.exists(folder)==FALSE){
    dir.create(folder)
  }
  if(dir.exists(paste0(folder,'/Results'))==FALSE){
    dir.create(paste0(folder,'/Results'))
  }
  # will save data in appropriate folder, based on 'animal' argument
  Part::Part_calcmoveStats(dat = dat, projstring = projstring,
                     dataset = dataset, time.zone = time.zone, ncpus = ncpus,saveby=saveby,
                     folder=folder,imp=imp)
  cat('Finished with MoveStats:',format(Sys.time(),'%m/%d/%Y %H:%M'))
  if(saveby=='Animal'){
    datatype<-'Animal'
  }else{
    if(saveby=='Study'){
      datatype<-'Study'
    }
  }

  # n.boot<-prepBoots
  # nrun <- vector('list',n.boot)
  # for(j in 1:length (nrun)){nrun[[j]] <- dataset}
  # cl<-snow::makeSOCKcluster(rep('localhost',ncpus))
  # snow::clusterEvalQ(cl,c(library(randomForest),library(dplyr),require(rfUtilities)))
  prep<-Part::Part_caretModPrep(folder=folder,datatype=datatype,mean_date=mean_date,
                                bday_dat=bday_dat,idname=idname,sampsize=sampsize,ncpus=ncpus)
  # snow::stopCluster(cl)
  saveRDS(prep, file=paste0(folder,'/Results/ModPrepOutput_',gsub('-','',Sys.Date()),'.RDS'))
  cat("\n")
  cat('Finished with ModPrep:',format(Sys.time(),'%m/%d/%Y %H:%M'))
  #prep the vars for the model runs
#
#
#   vars<-Part::Part_varPrep(prep)
#
#
#   saveRDS(vars, file=paste0(folder,'/Results/varPrepOutput_',gsub('-','',Sys.Date()),'.RDS'))
#   cat("\n")
#   cat('Finished with VarPrep:',format(Sys.time(),'%m/%d/%Y %H:%M'))
#
#   #tune the mtry parameter. This is done PRIOR to the modTrain function
#   #because it is essentially repeatedly doing this for no reason.
#   mtry<-Part::Part_mt(prep[[1]][[1]],vars)
#   saveRDS(mtry, file=paste0(folder,'/Results/mtOutput_',gsub('-','',Sys.Date()),'.RDS'))
#   cat("\n")
#   cat('Finished with MT:',format(Sys.time(),'%m/%d/%Y %H:%M'))
#   # runs model a single time
#   #system.time({ strain<-modTrain(data=prep[[1]][[1]],vars=vars,sampsize=70) })


  # runs model in parallel for MONSTER
  #2.3 hours NewElk, 1000 bootstraps, 20 cores
  n.boot<-finBoots
  nrun <- vector('list',n.boot)
  for(j in 1:length(nrun)){nrun[[j]] <- dataset}
  cl<-snow::makeSOCKcluster(rep('localhost',ncpus))
  snow::clusterEvalQ(cl,c(library(randomForest),library(dplyr)))
  strain<-snow::parLapply(cl,nrun,fun=Part::Part_modTrain,vars=prep[[2]],mt=prep[[4]],sampsize=sampsize,data=prep[[1]])
  snow::stopCluster(cl)
  #saveRDS(strain, file='C:/Users/mhayes1/Desktop/CAElk_modtrain200_05062016.RDS')
  saveRDS(strain, file=paste0(folder,'/Results/ModTrainOutput_',gsub('-','',Sys.Date()),'.RDS'))
  cat("\n")
  cat('Finished with ModTrain:',format(Sys.time(),'%m/%d/%Y %H:%M'))
  # get bootstrapped rf models
  rf <- list()
  for(i in 1:length(strain)){
    rf[[i]] <- strain[[i]][[1]]
  }
  saveRDS(rf, file=paste0(folder,'/Results/RFModsForPredict_',gsub('-','',Sys.Date()),'.RDS'))
  cl<-snow::makeSOCKcluster(rep('localhost',ncpus))
  snow::clusterEvalQ(cl,c(library(randomForest),library(dplyr)))
  # uses each RF to predict out to all animals
  nstrain<-snow::parLapply(cl,rf,fun=Part::Part_nPred,folder=folder,datatype=datatype,
                     mean_date=mean_date,idname='UAID',timename='time')
  snow::stopCluster(cl)
  saveRDS(nstrain, file=paste0(folder,'/Results/nPredOutput_',gsub('-','',Sys.Date()),'.RDS'))
  cat("\n")
  cat('Finished with nPred:',format(Sys.time(),'%m/%d/%Y %H:%M'))
  #saveRDS(nstrain, file='C:/Users/mhayes1/Desktop/CAElk_nPred.RDS')
  # pulling out raw data results
  #Mean value is Actual minus Predicted
  #negative values are predicting late, positive are predicting early
  res<-Part::Part_predResults(strain) # this one aggregates the model training - prediction results
  nres<-Part::Part_npredResults(nstrain) # this one aggregates the predicted results to make a final prediction

  saveRDS(res, file=paste0(folder,'/Results/TrainingPredictions_',gsub('-','',Sys.Date()),'.RDS'))
  saveRDS(nres, file=paste0(folder,'/Results/FinalPredictions_',gsub('-','',Sys.Date()),'.RDS'))
  cat("\n\n\n")
  cat('Parturition Model Ran in', round(difftime(Sys.time(),ab,units='mins'),digits=3),'minutes')
  # save NRES as final_results
  #saveRDS(nres, file="/Users/efuller/Google Drive/Parturition/FINAL/02202016_NewElk_BFHElk_FinalOutputs.RDS")
  return(list(res,nres))
}
