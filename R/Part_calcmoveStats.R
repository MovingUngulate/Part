#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param dat data
#' @param projstring Experimental, leave as default (FALSE)
#' @param dataset Short name of dataset without space ('SMElk')
#' @param time.zone POSIX format timezone
#' @param folder character folder path ('/home/user/folder')
#' @param ncpus Number of cpus to use for parallel processing (minimum 2)
#' @param saveby Character vector, either "Study" to save a single file of all data or
#' "Animal" to save one file for each individual in your dataset
#' @param type Character vector, either 'Norm' for normal running or 'NPred' for new predictions.
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
Part_calcmoveStats <- function(dat, projstring, dataset, time.zone,folder,ncpus=ncpus,saveby='Study',type='Norm',imp=FALSE){
  library(missForest,quietly = TRUE,verbose=FALSE)
  if(type=='Norm'){
    # calculate brownian bridge statistics
    dyn_dat <- Part::Part_BGBFun(data = dat, xname = "x", yname = "y", timename = "TelemDate", idname = "UAID", projstring = projstring, ncpus = ncpus)
    # save intermediate file becuase it takes a long time
    if(dir.exists(folder)==FALSE){
      dir.create(folder)
    }
    dir.create(paste0(folder,'/DataPrep'))
    save(dyn_dat, file=paste0(paste0(folder,'/DataPrep'),"/DataPrep_",dataset,'_',gsub('-','',Sys.Date()),".Rdata"))

    # Prep and run the trajfunction
    dat$UAID<-paste('X',dat$UAID,sep='')
    ds<-split(dat,as.factor(dat$UAID))

    cl<-snow::makeSOCKcluster(rep('localhost',ncpus))
    snow::clusterEvalQ(cl,library(adehabitatLT))
    testraj<-snow::parLapply(cl,ds,fun=Part_trajfun,timename='TelemDate',idname='UAID',xname='x',yname='y')
    snow::stopCluster(cl)

    #fix up traj output
    ti <- as.data.frame(data.table::rbindlist(testraj))
    ti$id<-as.character(ti$id)

    #Run Binding function
    outtra<-Part::Part_bindfun(ti=ti,outfun=dyn_dat)

    # prep and run rollstats

    # add columns for day type
    # set up vector of coords/time for sunrise sunset calc
    loc_times <- sp::SpatialPoints(outtra[,c("x", "y")], proj4string =
                                 sp::CRS(projstring))

    # need to be projected to lat/lon
    latlon <- sp::spTransform(loc_times,sp::CRS("+proj=longlat +ellps=clrk66"))

    # returns time of sunrise/sunset according to UTC (same tz as input time)
    up <- maptools::sunriset(latlon, outtra$date,  direction = "sunrise", POSIXct.out=TRUE)
    down <- maptools::sunriset(latlon, outtra$date, direction = "sunset",POSIXct.out = TRUE)

    outtra$sunrise <- up$time
    outtra$sunset <- down$time
    # if time is > sunrise and < sunset = daylight
    outtra$daylight <- with(outtra, ifelse(date > sunrise & date < sunset,
                                           1, 0))

    # if time is within 1 hour of sunset (either side) = twilight
    outtra$twilight <- with(outtra, ifelse(date < sunset + 60*60 &
                                             date > sunset - 60*60,
                                           1, 0))

    # if time is within 1 hour of sunrise (either side) = dawn
    outtra$dawn <- with(outtra, ifelse(date < sunrise + 60*60 &
                                         date > sunrise - 60*60,
                                       1, 0))

    #outtra$crepuscular <- with(outtra, ifelse(dawn ==1 | twilight == 1, 1, 0))
    outtra$night <- with(outtra, ifelse(daylight == 0, 1, 0))

    #Fix up bound output
    outtra2 <- outtra %>%
      dplyr::select(-burst, -pkey) %>%
      dplyr::rename(UAID = id) %>%
      dplyr::rename(time = date)



    #make sure time is in posix form
    outtra2$time<-as.POSIXct(as.character(outtra2$time),format='%Y-%m-%d %H:%M:%S',tz = time.zone)

    #Prep and run the rollfun
    outtra2<-split(outtra2,as.factor(outtra2$UAID))

    cl<-snow::makeSOCKcluster(rep('localhost',ncpus))
    snow::clusterEvalQ(cl,c(library(zoo),library(sp)))
    system.time(outRoll<-snow::parLapply(cl,outtra2,fun=Part::Part_rollfun, original = FALSE))
    snow::stopCluster(cl)

    #fix up rollfun output
    outRoll <- as.data.frame(data.table::rbindlist(outRoll))

    if(imp == TRUE){
    cluster <- snow::makeCluster(ncpus) # convention to leave 1 core for OS
    doParallel::registerDoParallel(cluster)

    outRoll[,(40:ncol(outRoll))] <- missForest::missForest(outRoll[,(40:ncol(outRoll))],ntree=ncpus,parallelize='forests',maxiter = 5)[[1]]
    snow::stopCluster(cluster)
    foreach::registerDoSEQ()
    }
    outRoll$dt<-ifelse(is.na(outRoll$dt),1,outRoll$dt)
    outRoll[,(40:ncol(outRoll))]<-outRoll[,(40:ncol(outRoll))]/outRoll$dt

    #outRoll[,(21:ncol(outRoll))]<-ifelse(is.na(outRoll$dt),outRoll[,(21:ncol(outRoll))],outRoll[,(21:ncol(outRoll))]/outRoll$dt)

    # if(dir.exists(folder)==FALSE){
    #   dir.create(folder)
    # }
    dir.create(paste0(folder,'/ReadyData'))
    if(saveby=='Animal'){
      # save final dataset ----
      # split into each summer's path as a file to make them small enough for github
      out_list <- split(outRoll, outRoll$UAID)
      for(i in 1:length(out_list)){
        saveRDS(out_list[[i]], file = paste0(paste0(folder,'/ReadyData/'),dataset,unique(out_list[[i]]$UAID),".RDS"))
      }
    }else{
      if(saveby=='Study'){
        saveRDS(outRoll,file = paste0(paste0(folder,'/ReadyData/'),'ModReady_',dataset,'_',gsub('-','',Sys.Date()),".RDS"))
      }
    }

  }else{


    if(type=='NPred'){
      dyn_dat <- Part::Part_BGBFun(data = dat, xname = "x", yname = "y", timename = "TelemDate", idname = "UAID", projstring = projstring, ncpus = ncpus)
      # save intermediate file becuase it takes a long time
      if(dir.exists(paste0(folder,'/NewData'))==FALSE){
        dir.create(paste0(folder,'/NewData'))
      }
      #dir.create(paste0(folder,'/NewData'))
      save(dyn_dat, file=paste0(paste0(folder,'/NewData'),"/NewData",dataset,'_',gsub('-','',Sys.Date()),".Rdata"))

      # Prep and run the trajfunction
      dat$UAID<-paste('X',dat$UAID,sep='')
      ds<-split(dat,as.factor(dat$UAID))

      cl<-snow::makeSOCKcluster(rep('localhost',ncpus))
      snow::clusterEvalQ(cl,library(adehabitatLT))
      testraj<-snow::parLapply(cl,ds,fun=Part::Part_trajfun,timename='TelemDate',idname='UAID',xname='x',yname='y')
      snow::stopCluster(cl)

      #fix up traj output
      ti <- as.data.frame(data.table::rbindlist(testraj))
      ti$id<-as.character(ti$id)

      #Run Binding function
      outtra<-Part::Part_bindfun(ti=ti,outfun=dyn_dat)

      # prep and run rollstats

      # add columns for day type
      # set up vector of coords/time for sunrise sunset calc
      loc_times <- sp::SpatialPoints(outtra[,c("x", "y")], proj4string =
                                   sp::CRS(projstring))

      # need to be projected to lat/lon
      latlon <- sp::spTransform(loc_times,sp::CRS("+proj=longlat +ellps=clrk66"))

      # returns time of sunrise/sunset according to UTC (same tz as input time)
      up <- maptools::sunriset(latlon, outtra$date,  direction = "sunrise", POSIXct.out=TRUE)
      down <- maptools::sunriset(latlon, outtra$date, direction = "sunset",POSIXct.out = TRUE)

      outtra$sunrise <- up$time
      outtra$sunset <- down$time
      # if time is > sunrise and < sunset = daylight
      outtra$daylight <- with(outtra, ifelse(date > sunrise & date < sunset,
                                             1, 0))

      # if time is within 1 hour of sunset (either side) = twilight
      outtra$twilight <- with(outtra, ifelse(date < sunset + 60*60 &
                                               date > sunset - 60*60,
                                             1, 0))

      # if time is within 1 hour of sunrise (either side) = dawn
      outtra$dawn <- with(outtra, ifelse(date < sunrise + 60*60 &
                                           date > sunrise - 60*60,
                                         1, 0))

      outtra$crepuscular <- with(outtra, ifelse(dawn ==1 | twilight == 1, 1, 0))
      outtra$night <- with(outtra, ifelse(daylight == 0, 1, 0))

      #Fix up bound output
      outtra2 <- outtra %>%
        dplyr::select(-burst, -pkey) %>%
        dplyr::rename(UAID = id) %>%
        dplyr::rename(time = date)

      #make sure time is in posix form
      outtra2$time<-as.POSIXct(as.character(outtra2$time),format='%Y-%m-%d %H:%M:%S',tz = time.zone)

      #Prep and run the rollfun
      outtra2<-split(outtra2,as.factor(outtra2$UAID))

      cl<-snow::makeSOCKcluster(rep('localhost',ncpus))
      snow::clusterEvalQ(cl,c(library(zoo),library(sp)))
      system.time(outRoll<-snow::parLapply(cl,outtra2,fun=Part::Part_rollfun, original = FALSE))
      snow::stopCluster(cl)

      #fix up rollfun output
      outRoll <- as.data.frame(data.table::rbindlist(outRoll))

      if(imp == TRUE){
      cluster <- snow::makeCluster(ncpus) # convention to leave 1 core for OS
      doParallel::registerDoParallel(cluster)
      outRoll[,(40:ncol(outRoll))] <- missForest::missForest(outRoll[,(40:ncol(outRoll))],ntree=ncpus,parallelize='forests')[[1]]

      snow::stopCluster(cluster)
      foreach::registerDoSEQ()
      }
      outRoll[,(40:ncol(outRoll))]<-outRoll[,(40:ncol(outRoll))]/outRoll$dt
      return(outRoll)
    }
  }
}
