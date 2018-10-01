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
Part_modPrep <- function(nrun,folder,datatype,mean_date,bday_dat,idname='UAID',sampsize=80){
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
  birth_dat <- foo %>%
    dplyr::select(UAID, Date.of.Birth) %>%
    distinct() %>%
    mutate(doy_birth = as.numeric(format(Date.of.Birth, "%j"))) %>%
    #mutate(UAID = paste0("X", UAID)) %>%
    right_join(dat) %>%
    mutate(doy = as.numeric(format(time, "%j")), birth = ifelse(doy==doy_birth, 1, 0),
           month = as.numeric(format(time, "%m")))%>%
    filter(doy > mean_date-30, doy < mean_date+30) %>%
    dplyr::select(-month)

  birth_dat <- as.data.frame(birth_dat)

  # find arguments for function
  # remove any columns that are all NA, these are due to fact that some intervals are > 3 hours,
  # so can't calculate sd on them (only get one point per statistic).
  predictors <- colnames(birth_dat)
  predictors <- predictors[-which(predictors %in% c("UAID","Date.of.Birth","doy_birth","x","y","time","dx","dy","dt","sunrise","sunset", "birth", "doy"))]
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

  modpar = NA

  while(is.na(modpar)){
    # sometimes the rf.modelSel() fails to choose a model, and returns no test
    # values. it seems dependent on a subset of animals in combination. this
    # while statement catches the problem and samples a new group of animals to
    # try again.

    #randomly grab n.animals of UAIDs
    samp<-sample(uni, n.animals)

    #create model training birth_dat from sample subset
    trainbirth_dat<-birth_dat[which(birth_dat[,idname] %in% samp),]

    #create model validation birth_dat from sample subset
    testbirth_dat<-birth_dat[which(!birth_dat[,idname] %in% samp),]

    #make partcode a factor
    trainbirth_dat[,'birth']<-as.factor(as.character(trainbirth_dat[,'birth']))

    #go through Evans et al model selection code
    modsel<-rfUtilities::rf.modelSel(trainbirth_dat[,predictors],trainbirth_dat[,'birth'],r=c(0.1, 0.2, 0.5, 0.7, 0.9),plot.imp=F)

    #record number of parameters in best model
    modpar<-modsel$test[1,4]
  }

  #create list of models
  modlist<-modsel$parameters

  #search through the models for the one with correct # parameters
  #and record the model number
  for(i in 1:length(modlist)){
    bl<-length(modlist[[i]])
    gmod<-ifelse(modpar == bl,i,next)
  }

  #record variables used in best model
  vars <- modsel$parameters[[gmod]]

  return(list(birth_dat,vars,modsel$importance))
}
