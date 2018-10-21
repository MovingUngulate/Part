#' @title Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#
#' @description Wrapper function to create all variables necessary to create ungulate parturition
#' prediction models.
#'
#' @param nrun folder where ready data is stored '/home/user/Desktop/Partmod'
#' @param folder folder where ready data is stored '/home/user/Desktop/Partmod'
#' @param datatype Either 'Animal' if files for each animal or 'Study' if one file saved
#' @param mean_date julian day (as.numeric) of the mean date of parturition for your animal/area
#' @param bday_dat data.frame with two columns, UAID (character) and Date.of.Birth (as.Date)
#' @param idname character vector name of UAID column
#' @param sampsize percent of dataset to use to train model
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
#' @importFrom dplyr "%>%"
Part_caretModPrep <- function(folder,datatype,mean_date,bday_dat,idname=idname,sampsize=sampsize,
                              ncpus,part_type=part_type,targ_type=targ_type,targ_dist=targ_dist){
  if(datatype=='Animal'){
    # load data
    #folder <- paste0("Analysis/02_Code/01_calc_move_stats/movement_stats_df/",animal,"/")
    in_files <- list.files(paste0(folder,'/ReadyData/'),pattern='.RDS$',full.names=T)
    dat <- lapply(in_files,readRDS)
    dat <- do.call(rbind, dat)
    dat$UAID <- as.character(dat$UAID) # change from factor
    dat$UAID<-gsub('X','',dat$UAID)
  }else{
    if(datatype=='Study'){
      in_files<-list.files(paste0(folder,'/ReadyData/'),pattern='.RDS$',full.names=T)
      dat<-readRDS(in_files)
      dat$UAID <- as.character(dat$UAID) # change from factor
      dat$UAID<-gsub('X','',dat$UAID)
    }
  }





  # add birthdays
  foo <- bday_dat

  dat<-dat[which(dat$UAID %in% foo$UAID),]
  # was getting bizarre index errors
  
  if(part_type == 'date'){
    birth_dat <- foo %>%
      dplyr::select(UAID, Date.of.Birth) %>%
      dplyr::distinct() %>%
      dplyr::mutate(doy_birth = as.numeric(format(Date.of.Birth, "%j"))) %>%
      #mutate(UAID = paste0("X", UAID)) %>%
      dplyr::right_join(dat,by='UAID') %>%
      dplyr::mutate(doy = as.numeric(format(time, "%j")), birth = ifelse(doy==doy_birth, 1, 0),
             month = as.numeric(format(time, "%m")))%>%
      dplyr::filter(doy > mean_date-30, doy < mean_date+30) %>%
      dplyr::select(-month)
  }
  
  if(part_type == 'dist'){
    birth_dat <- foo %>% 
      dplyr::select(UAID, Date.of.Birth) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(doy_birth = as.numeric(format(Date.of.Birth,"%j"))) %>% 
      dplyr::right_join(dat, by = "UAID") %>% 
      dplyr::mutate(doy = as.numeric(format(time, "%j")), month = as.numeric(format(time, "%m"))) %>% 
      dplyr::filter(doy > mean_date - 30, doy < mean_date + 30) %>% 
      dplyr::select(-month)

    birth_dat <- merge(birth_dat,foo,by=c('UAID','Date.of.Birth'),all.x=T)
  
    birth_dat$BirthDist = abs(sqrt( (birth_dat$Easting - birth_dat$x)^2 + (birth_dat$Northing - birth_dat$y)^2))
  
    birth_dat$birth = ifelse(birth_dat$BirthDist < targ_dist & abs(difftime(birth_dat$time,birth_dat$Date.of.Birth,units ='hours')) < targ_time,1,0)
  
    birth_dat = birth_dat[,c(1:(ncol(birth_dat)-4),ncol(birth_dat))]
  
  }
  birth_dat <- as.data.frame(birth_dat)

  # find arguments for function
  # remove any columns that are all NA, these are due to fact that some intervals are > 3 hours,
  # so can't calculate sd on them (only get one point per statistic).
  predictors <- colnames(birth_dat)
  predictors <- predictors[-which(predictors %in% c("UAID","Date.of.Birth","doy_birth","x","y","time","dx","dy","dt","sunrise","sunset",'daylight','twilight','dawn','night', "birth", "doy"))]
  # drop all R2n variables, worried about migration
  predictors <- predictors[-grep("R2n", predictors)]
  # change into index
  predictors <- unique(which(colnames(birth_dat) %in% predictors))

  # if a predictor has more than 100 NAs, drop from model
  # this comes up in CaliDeer because more than half of animals have a 3-4 hour fix rate

  all_NAs <- which(as.numeric(apply(birth_dat[,predictors], 2, function(X) length(which(is.na(X)))))>100)
  if(length(all_NAs)>0) {predictors = predictors[-all_NAs]}

  all_zeros <- as.numeric(which(apply(head(birth_dat[,predictors]),2,function(x)all(x==0))))
  if(length(all_zeros)>0) {predictors = predictors[-all_zeros]}


  #need only complete birth_dat
  birth_dat<-birth_dat[complete.cases(birth_dat[,predictors]),]

  #normalize covariates
  birth_dat[,predictors] <- lapply(birth_dat[,predictors], function(X) (X - min(X))/diff(range(X)))


  #create vector of unique animal ids
  uni<-unique(birth_dat[,idname])

  # translate percentage into integer number of ani
  n.animals = floor(length(uni)*(sampsize/100))



    #randomly grab n.animals of UAIDs
    samp<-sample(uni, n.animals)

    #create model training birth_dat from sample subset
    trainbirth_dat<-birth_dat[which(birth_dat[,idname] %in% samp),]

    #create model validation birth_dat from sample subset
    testbirth_dat<-birth_dat[which(!birth_dat[,idname] %in% samp),]

    #make partcode a factor
    trainbirth_dat[,'birth']<-as.factor(as.character(trainbirth_dat[,'birth']))



    ctrl <- caret::rfeControl(functions = caret::rfFuncs,
                              method = "cv",
                              number = 10,
                              verbose = TRUE)

    #if we have lots of cores, we only need 10
    fincpu<-ifelse(ncpus>10,10,ncpus)
    cluster <- snow::makeCluster(fincpu) # convention to leave 1 core for OS
    doParallel::registerDoParallel(cluster)
    system.time({rfProfile <- caret::rfe(x=trainbirth_dat[,predictors],
                                        y=trainbirth_dat[,'birth'],
                                        sizes = c(1:10,seq(20,length(predictors),20)),
                                        rfeControl = ctrl,
                                        metric='Accuracy')})
    snow::stopCluster(cluster)
    foreach::registerDoSEQ()

    cv.ctrl <- caret::trainControl(method = "cv",
                            number = 10,
                            #summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            allowParallel=T)

    vars<-rfProfile$optVariables

    tab<-rfProfile$variables
    nt<-aggregate(tab$Overall,by=list(tab$var),FUN=max)


    trainbirth_dat$BCode<-ifelse(as.character(trainbirth_dat$birth)==0,'No','Yes')
    trainbirth_dat$BCode<-as.factor(trainbirth_dat$BCode)

    cluster <- snow::makeCluster(ncpus) # convention to leave 1 core for OS
    doParallel::registerDoParallel(cluster)
    system.time({
        rf_tune <-caret::train(x=trainbirth_dat[,rfProfile$optVariables],
                        y=trainbirth_dat[,'BCode'],
                        method="rf",
                        trControl=cv.ctrl,
                        tuneLength=ifelse(length(rfProfile$optVariables)<10,length(rfProfile$optVariables),10),
                        verbose=T)
    })
    snow::stopCluster(cluster)
    foreach::registerDoSEQ()



  return(list(birth_dat,vars,nt,rf_tune$bestTune[[1]]))
}
