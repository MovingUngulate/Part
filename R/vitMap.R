#' @title Create Plots for Parturition Markdown
#
#' @description Use movement data to create plots for Parturition Markdown Document
#' @param locdat movement data
#' @param vidat VIT data
#' @param vhist Lookup table 
#' @param fold Folder to save plots in
#' @param spp Species working with
#' @param plotdataPath path ('C:/Users/mhayes1/Desktop/PlotData') to save plot data
#' @return Returns plots for markdown in specified folder
#' @keywords VIT, parturition, markdown
#' @export
#' @examples
#' \donttest{system.time({ vitMap(locdat=mdat2,vidat=vi,vhist=vhist,fold='/home/mhayes1/Desktop/DEERPTesting/plots',spp='deer') })
#' )}
#'

vitMap<-function(locdat,vidat,vhist,fold,spp,plotdataPath,hg=NULL){
  if (spp == "deer") {
    locdat$CollarSerialNumber <- as.character(as.numeric(as.character(locdat$CollarSerialNumber)))
    vidat$n <- ifelse(vidat$Event == "Birth Not yet triggered" | vidat$Event == 'not expelled', 
                      0, ifelse(vidat$Event == "Birth-triggered by lack of comm" | vidat$Event == 'expelled', 
                                1, 2))
    vidat$CollarSerialNumber <- as.character(as.numeric(vidat$CollarSerialNumber))
    vhist$Serial.Number <- as.character(as.numeric(vhist$Serial.Number))
    if(TRUE %in% (nchar(vhist$ActBD)>0)){
    vhist$ActBD<-as.POSIXct(paste0(vhist$ActBD,' 5'),'%m/%d/%Y %H',tz='MST')
    }
    uni <- unique(locdat$CollarSerialNumber)
    sub <- locdat
    sub <- sub[with(sub, order(sub[, "TelemDate"])), ]
    newdata <- adehabitatLT::as.ltraj(sub[, c("Easting", "Northing")], 
                                      sub[, "TelemDate"], id = sub[, "CollarSerialNumber"])
    fpt <- adehabitatLT::fpt(newdata, c(50, 100, 150, 200), "hours")
    out <- data.frame()
    for (k in 1:length(uni)) {
      sloc <- locdat[which(locdat$CollarSerialNumber == 
                             uni[k]), ]
      fptsub <- fpt[[k]]
      sloc$FPT50 <- fptsub$r1
      sloc$FPT100 <- fptsub$r2
      sloc$FPT150 <- fptsub$r3
      sloc$FPT200 <- fptsub$r4
      out <- rbind(out, sloc)
    }
    locdat <- out
    
    tim<-paste(strftime(Sys.time(),format='%Y'),'-05-01',sep='')
    locdat <- locdat[which(locdat$TelemDate >= as.POSIXct(tim, 
                                                          format = "%Y-%m-%d")), ]
    uni <- unique(locdat$CollarSerialNumber)
    uni <- uni[which(uni %in% as.character(vhist$Serial.Number))]
    allcks <- data.frame()
    for (l in 1:length(uni)) {
      sub <- locdat[which(locdat$CollarSerialNumber == 
                            uni[l]), ]
      
      sub<-sub[order(sub$TelemDate),]
      if(nrow(sub)<20){next}
      subvidat <- vidat[which(vidat$CollarSerialNumber == 
                                uni[l]), ]
      vhsub <- vhist[which(vhist$Serial.Number == uni[l]), 
                     ]
      subvidat <- subvidat[order(subvidat$Date, decreasing = T), 
                           ]
      tim<-paste(strftime(Sys.time(),format='%Y'),'-05-01 00:00:00',sep='')
      subvidat<-subvidat[which(subvidat$Date>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
      sub$FPT50 <- ifelse(is.na(sub$FPT50),0, sub$FPT50)
      sub$FPT100 <- ifelse(is.na(sub$FPT100), 
                           0, sub$FPT100)
      sub$FPT150 <- ifelse(is.na(sub$FPT150), 
                           0, sub$FPT150)
      sub$FPT200 <- ifelse(is.na(sub$FPT200), 
                           0, sub$FPT200)
      sub$MR <- sub$dist/sub$dt
      fn <- vhsub$Frequency[1]
      fn <- gsub(".", "", fn, fixed = T)
      if(nchar(fn)<6){
        fn<-paste0(fn,paste0(rep('0',6-nchar(fn)),collapse=''))
      }
      fn <- paste(fold, fn, sep = "/")
      fn <- paste(fn, "png", sep = ".")
      png(filename = fn, height = 1400, width = 1500, res = 75)
      par(mfrow = c(3, 2))
      
      if(nrow(subvidat)==0){
        plot.new()
      }
      
      if(nrow(subvidat)>0){
        tim<-paste(strftime(Sys.time(),format='%Y'),'-05-01 00:00:00',sep='')
        subvidat<-subvidat[which(subvidat$Date>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
        plot(subvidat$Date, subvidat$n, type = "b", pch = 20, 
             ylab = "", yaxt = "n", main = "Vit History", 
             xlab = "Date", ylim = c(0, 2))
        axis(side = 2, at = c(0, 1, 2), labels = c("NoBirth", 
                                                   "Comm", "Other"), las = 1, cex.axis = 1.15)
      abline(v=vhsub$ActBD[1],col='green',lty=2)
        }
      tim<-paste(strftime(Sys.time(),format='%Y'),'-05-01',sep='')
      sub <- sub[which(sub$TelemDate >= as.POSIXct(tim, 
                                                   format = "%Y-%m-%d")), ]
      plot(sub$TelemDate, sub$MR, type = "l", ylab = "Movement Rate", 
           xlab = "Date", main = "Movement Rate", cex = 1.25)
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      sub$MRM <- NA
      sub$MRM[12:nrow(sub)] <- zoo::rollmean(sub$MR, k = 12)
      if(nrow(sub)>48){
        mm <- quantile(sub$MR[(nrow(sub) - 48):(nrow(sub))], 
                       probs = 0.75)
      }else{
        mm <- quantile(sub$MR, 
                       probs = 0.75)
      }
      mc <- as.data.frame(table(sub$MR[(nrow(sub) - 12):(nrow(sub))] < 
                                  mm))
      mc <- mc[which(mc$Var1 == "TRUE"), ]
      mc <- ifelse(nrow(mc) == 0, 0, mc$Freq[1])
      lines(sub$TelemDate, sub$MRM, col = "red")
      abline(h = mm, col = "blue", lty = 2)
      plot(sub$TelemDate, sub$orthSd, type = "l", ylim = c(0, 
                                                           50), ylab = "Metric", xlab = "Date", main = "dBGB Metrics", 
           cex = 1.25)
      lines(sub$TelemDate, sub$paraSd, col = "blue")
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      plot(sub$TelemDate, sub$FPT50, type = "l", ylab = "FPT (hours)", 
           xlab = "Date", main = "FPT 50m radius", cex = 1.25)
      abline(h = quantile(sub$FPT50, na.rm = T)[4], col = "red")
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      tc <- quantile(sub$FPT50, na.rm = T)[4]
      fc1 <- as.data.frame(table(sub$FPT50[(nrow(sub) - 
                                              12):(nrow(sub))] > tc))
      fc1 <- fc1[which(fc1$Var1 == "TRUE"), ]
      fc1 <- ifelse(nrow(fc1) == 0, 0, fc1$Freq[1])
      plot(sub$TelemDate, sub$FPT100, type = "l", ylab = "FPT (hours)", 
           xlab = "Date", main = "FPT 100m radius", cex = 1.25)
      
      abline(h = quantile(sub$FPT100, na.rm = T)[4], col = "red")
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      tc <- quantile(sub$FPT100, na.rm = T)[4]
      fc2 <- as.data.frame(table(sub$FPT100[(nrow(sub) - 
                                               12):(nrow(sub))] > tc))
      fc2 <- fc2[which(fc2$Var1 == "TRUE"), ]
      fc2 <- ifelse(nrow(fc2) == 0, 0, fc2$Freq[1])
      # plot(sub$TelemDate, sub$FPT150, type = "l", ylab = "FPT (hours)", 
      #      xlab = "Date", main = "FPT 150m radius", cex = 1.25)
      # abline(h = quantile(sub$FPT150, na.rm = T)[4], col = "red")
      # 
      
      predsub<-hg[hg$CollarSerialNumber==uni[l],]
      
      tim<-paste(strftime(Sys.time(),format='%Y'),'-05-01 00:00:00',sep='')
      predsub<-predsub[which(predsub$TelemDate>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
      #predsub<-predsub[predsub>-]
      
      
      plot(predsub$TelemDate, predsub$Pred0, type = "l", ylab = "Probability", 
           xlab = "Date", main = "ML Predictions", cex = 1.25,ylim=c(0,1),lwd=1)
      lines(predsub$TelemDate,predsub$Pred2,col='red',lwd=1)
      lines(predsub$TelemDate,predsub$Pred1,col='blue',lwd=2)
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      
      tc <- quantile(sub$FPT150, na.rm = T)[4]
      fc3 <- as.data.frame(table(sub$FPT150[(nrow(sub) - 
                                               12):(nrow(sub))] > tc))
      fc3 <- fc3[which(fc3$Var1 == "TRUE"), ]
      fc3 <- ifelse(nrow(fc3) == 0, 0, fc3$Freq[1])
      
      if('AID' %in% names(vhsub)){
        mf <- paste(paste("Mom Freq: ", vhsub$Frequency[1], 
                          sep = " "),paste('AID: ',vhsub$AID[1],sep=''),sep=' ')
        
      }
      if(!('AID' %in% names(vhsub))){
        mf <- paste("Mom Freq: ", vhsub$Frequency[1], 
                    sep = " ")
      }
      vf <- paste("Vit Freq:", vhsub$VIT.Freq[1], sep = " ")
      mtext(mf, font = 2, side = 3, line = -2.25, outer = T, 
            cex = 2)
      mtext(vf, font = 2, side = 3, line = -50, outer = T, 
            cex = 2)
      dev.off()

      cks <- data.frame(Pred0Check=predsub$Pred0[nrow(predsub)],
                        Pred1Check=predsub$Pred1[nrow(predsub)],
                        Pred2Check=predsub$Pred2[nrow(predsub)],
                        stringsAsFactors = F)
      cks$Serial <- sub$CollarSerialNumber[1]
      allcks <- rbind(allcks, cks)
    }
    #allcks$RMean <- rowMeans(allcks[, 1:4])
    
    ppp<-paste0(plotdataPath,'_Deer.RDS')
    saveRDS(allcks, ppp)
  }
  if (spp == "elk") {
    locdat$CollarSerialNumber <- as.character(as.numeric(as.character(locdat$CollarSerialNumber)))
    vhist$Serial.Number <- as.character(as.numeric(vhist$Serial.Number))
    if(TRUE %in% (nchar(vhist$ActBD)>0)){
      vhist$ActBD<-as.POSIXct(paste0(vhist$ActBD,' 5'),'%m/%d/%Y %H',tz='MST')
    }
    locdat <- locdat[which(locdat$CollarSerialNumber %in% 
                             vhist$Serial.Number), ]
    uni <- unique(locdat$CollarSerialNumber)
    sub <- locdat
    sub <- sub[with(sub, order(sub[, "TelemDate"])), ]
    newdata <- adehabitatLT::as.ltraj(sub[, c("Easting", "Northing")], 
                                      sub[, "TelemDate"], id = sub[, "CollarSerialNumber"])
    fpt <- adehabitatLT::fpt(newdata, c(100, 250, 500, 750, 1000), "hours")
    out <- data.frame()
    for (k in 1:length(uni)) {
      sloc <- locdat[which(locdat$CollarSerialNumber == 
                             uni[k]), ]
      fptsub <- fpt[[k]]
      sloc$FPT50 <- fptsub$r1
      sloc$FPT100 <- fptsub$r2
      sloc$FPT150 <- fptsub$r3
      sloc$FPT200 <- fptsub$r4
      sloc$FPT300 <- fptsub$r5
      out <- rbind(out, sloc)
    }
    locdat <- out
    tim<-paste(strftime(Sys.time(),format='%Y'),'-05-01',sep='')
    locdat <- locdat[which(locdat$TelemDate >= as.POSIXct(tim, 
                                                          format = "%Y-%m-%d",tz='MST')), ]
    allcks <- data.frame()
    uni <- unique(locdat$CollarSerialNumber)
    for (l in 1:length(uni)) {
      sub <- locdat[which(locdat$CollarSerialNumber == 
                            uni[l]), ]
      if(nrow(sub)<20){next}
      vhsub <- vhist[which(vhist$Serial.Number == uni[l]), 
                     ]
      sub$FPT50[50:nrow(sub)] <- ifelse(is.na(sub$FPT50[50:nrow(sub)]), 
                                        0, sub$FPT50)
      sub$FPT100[50:nrow(sub)] <- ifelse(is.na(sub$FPT100[50:nrow(sub)]), 
                                         0, sub$FPT100)
      sub$FPT150[50:nrow(sub)] <- ifelse(is.na(sub$FPT150[50:nrow(sub)]), 
                                         0, sub$FPT150)
      sub$FPT300[50:nrow(sub)] <- ifelse(is.na(sub$FPT300[50:nrow(sub)]), 
                                         0, sub$FPT300)
      sub$MR <- sub$dist/sub$dt
      fn <- vhsub$Frequency[1]
      fn <- gsub(".", "", fn, fixed = T)
      if(nchar(fn)<6){
        fn<-paste0(fn,paste0(rep('0',6-nchar(fn)),collapse=''))
        }
      fn <- paste(fold, fn, sep = "/")
      fn <- paste(fn, "png", sep = ".")
      png(filename = fn, height = 1400, width = 1500, res = 75)
      par(mfrow = c(3, 2))
      
      tim<-paste(strftime(Sys.time(),format='%Y'),'-04-01',sep='')
      sub <- sub[which(sub$TelemDate >= as.POSIXct(tim, 
                                                   format = "%Y-%m-%d")), ]
      plot(sub$TelemDate, sub$MR, type = "l", ylab = "Movement Rate", 
           xlab = "Date", main = "Movement Rate", cex = 1.25)
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      sub$MRM <- NA
      sub$MRM[12:nrow(sub)] <- zoo::rollmean(sub$MR, k = 12)
      mm <- quantile(sub$MR[(nrow(sub) - 48):(nrow(sub))], 
                     probs = 0.75)
      mc <- as.data.frame(table(sub$MR[(nrow(sub) - 12):(nrow(sub))] < 
                                  mm))
      mc <- mc[which(mc$Var1 == "TRUE"), ]
      mc <- ifelse(nrow(mc) == 0, 0, mc$Freq[1])
      lines(sub$TelemDate, sub$MRM, col = "red")
      abline(h = mm, col = "blue", lty = 2)
      plot(sub$TelemDate, sub$orthSd, type = "l", ylim = c(0, 
                                                           50), ylab = "Metric", xlab = "Date", main = "dBGB Metrics", 
           cex = 1.25)
      lines(sub$TelemDate, sub$paraSd, col = "blue")
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      plot(sub$TelemDate, sub$FPT50, type = "l", ylab = "FPT (hours)", 
           xlab = "Date", main = "FPT 50m radius", cex = 1.25)
      abline(h = quantile(sub$FPT50, na.rm = T)[4], col = "red")
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      tc <- quantile(sub$FPT50, na.rm = T)[4]
      fc1 <- as.data.frame(table(sub$FPT50[(nrow(sub) - 
                                              12):(nrow(sub))] > tc))
      fc1 <- fc1[which(fc1$Var1 == "TRUE"), ]
      fc1 <- ifelse(nrow(fc1) == 0, 0, fc1$Freq[1])
      plot(sub$TelemDate, sub$FPT100, type = "l", ylab = "FPT (hours)", 
           xlab = "Date", main = "FPT 100m radius", cex = 1.25)
      abline(h = quantile(sub$FPT100, na.rm = T)[4], col = "red")
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      tc <- quantile(sub$FPT100, na.rm = T)[4]
      fc2 <- as.data.frame(table(sub$FPT100[(nrow(sub) - 
                                               12):(nrow(sub))] > tc))
      fc2 <- fc2[which(fc2$Var1 == "TRUE"), ]
      fc2 <- ifelse(nrow(fc2) == 0, 0, fc2$Freq[1])
      plot(sub$TelemDate, sub$FPT150, type = "l", ylab = "FPT (hours)", 
           xlab = "Date", main = "FPT 150m radius", cex = 1.25)
      abline(h = quantile(sub$FPT150, na.rm = T)[4], col = "red")
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      tc <- quantile(sub$FPT150, na.rm = T)[4]
      fc3 <- as.data.frame(table(sub$FPT150[(nrow(sub) - 
                                               12):(nrow(sub))] > tc))
      fc3 <- fc3[which(fc3$Var1 == "TRUE"), ]
      fc3 <- ifelse(nrow(fc3) == 0, 0, fc3$Freq[1])
      
      
      predsub<-hg[hg$CollarSerialNumber==uni[l],]
      
      tim<-paste(strftime(Sys.time(),format='%Y'),'-05-01 00:00:00',sep='')
      predsub<-predsub[which(predsub$TelemDate>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
      #predsub<-predsub[predsub>-]
      
      
      plot(predsub$TelemDate, predsub$Pred0, type = "l", ylab = "Probability", 
           xlab = "Date", main = "ML Predictions", cex = 1.25,ylim=c(0,1),lwd=1)
      lines(predsub$TelemDate,predsub$Pred2,col='red',lwd=1)
      lines(predsub$TelemDate,predsub$Pred1,col='blue',lwd=2)
      abline(v=vhsub$ActBD[1],col='green',lty=2)
      #abline(h = quantile(sub$FPT300, na.rm = T)[4], col = "red")
      tc <- quantile(sub$FPT300, na.rm = T)[4]
      fc4 <- as.data.frame(table(sub$FPT300[(nrow(sub) - 
                                               12):(nrow(sub))] > tc))
      fc4 <- fc4[which(fc4$Var1 == "TRUE"), ]
      fc4 <- ifelse(nrow(fc4) == 0, 0, fc4$Freq[1])
      mf <- paste("Mom Freq:", vhsub$Frequency[1], 
                  sep = " ")
      vf <- paste("Vit Freq:", vhsub$VIT.Freq[1], sep = " ")
      mtext(mf, font = 2, side = 3, line = -2.25, outer = T, 
            cex = 2)
      mtext(vf, font = 2, side = 3, line = -50, outer = T, 
            cex = 2)
      dev.off()
      cks <- data.frame(Pred0Check=predsub$Pred0[nrow(predsub)],
                        Pred1Check=predsub$Pred1[nrow(predsub)],
                        Pred2Check=predsub$Pred2[nrow(predsub)],
                        stringsAsFactors = F)
      cks$Serial <- sub$CollarSerialNumber[1]
      allcks <- rbind(allcks, cks)
    }
    #allcks$RMean <- rowMeans(allcks[, 1:5])
    
    ppp<-paste0(plotdataPath,'_Elk.RDS')
    saveRDS(allcks, ppp)
  }
}
