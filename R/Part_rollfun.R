#' @title Parturition Rolling Variable Creation
#
#' @description Creates rolling candidate variables for ungulate partition prediction.
#' Internally used in parturition methodology.
#' @param sub data
#' @param original Experimental, leave as default (FALSE)
#' @return Original data with rolling candidate variables
#' @keywords part, parturition
#' @export
Part_rollfun<-function(sub, original = FALSE){ # takes individuals trajectory
  # original = true means to just do without considering time of day.
  sub<-sub[order(sub$time),]
  sub<-sub[complete.cases(sub),]
  # vector for all times to fit machinary of daylight restricted rolling windows
  nc<-ncol(sub)
  sub$dt2<-NA
  sub$dt2[2:(nrow(sub))]<-sub$dt[1:(nrow(sub)-1)]
  sub$dt<-sub$dt2

  sub<-sub[,1:nc]
  sub$dt<-sub$dt/3600


  # set up functions to go through time windows
  make_time_windows <- function(window.size, data = sub){
    # window.size in hours

    # function to get data in list form for each sliding window of window.size
    win.size <- 60*60*window.size # convert to seconds

    # make a list for values in each time window
    time_dat <- data.frame(start = data$time)
    time_dat$end <- data$time + win.size

    time_list <- list()

    # have colvars to retain
    colvars <- c("dist","R2n","abs.angle","rel.angle","paraSd","orthSd")

    for(i in 1:nrow(data)){
      # find data in possible time interval
      time_list[[i]] <- data[which(data$time >= time_dat$start[i] &
                                     data$time<= time_dat$end[i]),colvars]

      if(nrow(time_list[[i]])==0){
        time_list[[i]] <- as.data.frame(t(
          data.frame(data = rep(NA, length(colvars)), row.names = colvars)
        ))
      }
    }

    # lapply to list entry to get basic stats by column for window size
    basic_stats <- function(x){
      if(all(is.na(x))){
        mn <- NA; md <- NA; sd <- NA; sum <- NA; max <- NA
       # mn <- NA; md <- NA; min<-NA; sum <- NA; max <- NA
      }else{
        mn <- mean(x, na.rm = T)
        md <- median(x, na.rm=T)
        sd <- sd(x, na.rm=T)
        #min <- min(x, na.rm=T)
        sum <- sum(x, na.rm = T)
        max <- max(x, na.rm = T)
      }

      return(list(mn, md, sd, sum, max))
    }

    # list entry for each data point, each entry is a list of 6 lists
    # (one for each measurement, R2n, dist, etc.)
    # and in each list is a list of 5, one for each of the functions in basic_stats()
    stats_3 <- lapply(time_list, function(x) apply(x, 2,basic_stats))

    # so to unlist, want nrows = nrow(data),
    # there will be nfunctions * data types (5*6) columns
    # df_stats <- matrix(unlist(stats_3),
    #                    nrow = nrow(data), byrow=TRUE, ncol = 5*length(colvars))
    #
    # # rename based on contents
    # colnames(df_stats) <- paste(rep(colvars, each=5),
    #                             rep(c("mean","median","sd",
    #                                   "sum","max"), 6),
    #                             window.size,sep="_")
    df_stats <- matrix(unlist(stats_3),
                       nrow = nrow(data), byrow=TRUE, ncol = 5*length(colvars))

    # rename based on contents
    colnames(df_stats) <- paste(rep(colvars, each=5),
                                rep(c("mean","median","sd",
                                      "sum","max"), 6),
                                window.size,sep="_")

    return(df_stats)
  }

  # get all combinations of parameters
  time_windows = c(4, 12, 24, 48, 96, 192)

  # run function
  hourly_rolls <-  mapply(make_time_windows, window.size = time_windows, SIMPLIFY = FALSE)

  outdata <- do.call(cbind, hourly_rolls) # put all data together
  outdata<-cbind(sub, outdata) # add to original data

  # do displacement
  sp::coordinates(outdata)<- ~x+y
  displacement_window <- function(window.size, outdata2 = outdata){
    # make a list for values in each time window
    win.size = 60*60*window.size
    time_dat <- data.frame(start = outdata2$time)
    time_dat$end <- outdata2$time + win.size

    time_list <- list()

    # have colvars to retain
    colvars = c("x","y")
    for(i in 1:(nrow(outdata2@data)-1)){
      # find data in possible time interval
      time_list[[i]] <- sp::coordinates(outdata2)[which(outdata2@data$time >= time_dat$start[i] &
                                                      outdata2@data$time<= time_dat$end[i]),colvars,drop=FALSE]
      # if nothing is returned, return NA and go to next iteration
      if(nrow(time_list[[i]])<=1) {
        time_list[[i]] <- NA
      } else {
        # take first and last coordinates
        time_list[[i]] <- sp::spDistsN1(time_list[[i]][1,,drop=FALSE],
                                    time_list[[i]][nrow(time_list[[i]]),,drop=FALSE])
      }
    }

    return(unlist(time_list))

  }
  displacement_rolls <- mapply(displacement_window, window.size = time_windows, SIMPLIFY = FALSE)

  displacement_df <- do.call(cbind, displacement_rolls)
  colnames(displacement_df) <- paste("displacement", time_windows, sep="_")
  displacement_df <- rbind(displacement_df,rep(NA, ncol(displacement_df)))

  outdata <- as.data.frame(outdata)
  outdata <- cbind(outdata, displacement_df)

  return(outdata)
}
