#' @title Create Tables for Parturition Markdown
#
#' @description Use movement and VIT data to create tables for Parturition Markdown Document
#' @param vhist Lookup table
#' @param mlist Character vector of mortality serials
#' @param vi Collar Download Status data
#' @param viout VIT Location data
#' @param outtra movement data
#' @param spp Species working with
#' @return Returns plots for markdown in specified folder
#' @keywords VIT, parturition, markdown
#' @export
#' @examples
#' \donttest{tabby<-tabFun(vhist=vhist,mlist=mlist,vi=vi,viout=viout,outtra=mdat2,spp='deer')}
#'

PartTab<-function(vhist,mlist,vi,viout,outtra,spp){
  if (spp == "deer") {
    vi <- vi[which(as.character(as.numeric(vi$CollarSerialNumber)) %in% 
                     vhist$Serial.Number), ]
    cursas <- vi
    cs <- data.frame()
    uni <- unique(cursas$CollarSerialNumber)
    for (i in 1:length(uni)) {
      s <- cursas[which(cursas$CollarSerialNumber == uni[i]), 
                  ]
      s <- s[which(s$Date == max(s$Date)), ]
      cs <- rbind(cs, s)
    }
    ntab <- vhist
    ntab$Mortality <- NA
    ntab <- ntab[which(!(ntab$Serial.Number %in% mlist)), 
                 ]
    vidat <- vi
    vidat$CollarSerialNumber <- as.character(as.numeric(as.character(vidat$CollarSerialNumber)))
    uni <- unique(vidat$CollarSerialNumber)
    agg <- data.frame()
    for (l in 1:length(uni)) {
      s <- vidat[which(vidat$CollarSerialNumber == uni[l]), 
                 ]
      s <- s[complete.cases(s$Event), ]
      if (nrow(s) == 0) {
        next
      }
      count <- 0
      for (y in 2:nrow(s)) {
        if (s$Event[y] == s$Event[y - 1]) {
          next
        }
        else {
          count <- count + 1
        }
      }
      jk <- data.frame(CollarSerialNumber = s$CollarSerialNumber[1], 
                       VitStatusChanges = count, stringsAsFactors = F)
      agg <- rbind(agg, jk)
    }
    ntab <- merge(ntab, agg, by.x = "Serial.Number", by.y = "CollarSerialNumber")
    visub <- vi
    visub$n <- ifelse(visub$Event == "Birth-Not yet triggered", 
                      0, 1)
    md <- Sys.time()
    md <- md - (86400 * 10)
    visub <- visub[which(visub$Date >= md), ]
    visub$CollarSerialNumber <- as.character(as.numeric(as.character(visub$CollarSerialNumber)))
    uni <- unique(visub$CollarSerialNumber)
    agg <- data.frame()
    for (l in 1:length(uni)) {
      s <- visub[which(visub$CollarSerialNumber == uni[l]), 
                 ]
      s <- s[complete.cases(s$Event), ]
      if (nrow(s) == 0) {
        next
      }
      if (nrow(s) == 1) {
        count <- 1
      }
      if(nrow(s)>1){
        count <- 0
        for (y in 2:nrow(s)) {
          if (s$Event[y] == s$Event[y - 1]) {
            next
          }
          else {
            count <- count + 1
          }}
      }
      jk <- data.frame(CollarSerialNumber = s$CollarSerialNumber[1], 
                       VitStatusChanges_10Day = count, stringsAsFactors = F)
      agg <- rbind(agg, jk)
    }
    ntab <- merge(ntab, agg, by.x = "Serial.Number", by.y = "CollarSerialNumber")
    viots <- viout
    viots <- viots[which(!(viots$CollarSerialNumber %in% 
                             mlist)), ]
    viots <- viots[complete.cases(viots$CollarSerialNumber), 
                   ]
    uni <- unique(viots$CollarSerialNumber)
    tl <- data.frame()
    for (i in 1:length(uni)) {
      vsub <- viots[which(viots$CollarSerialNumber == uni[i]), 
                    ]
      v <- vsub[which(vsub$Date == max(vsub$Date, na.rm = T)), 
                ]
      vsub <- vsub[which(!(vsub$Event == v$Event[1])), 
                   ]
      if (nrow(vsub) == 0) {
        tl <- rbind(tl, v)
        next
      }
      else {
        vsub <- vsub[which(vsub$Date == max(vsub$Date, 
                                            na.rm = T)), ]
        v$Easting <- vsub$Easting
        v$Northing <- vsub$Northing
        tl <- rbind(tl, v)
      }
    }
    tl <- tl[, c(1, 22:23)]
    tl$CollarSerialNumber <- as.numeric(tl$CollarSerialNumber)
    ntab <- merge(ntab, tl, by.x = "Serial.Number", by.y = "CollarSerialNumber")
    cs <- cs[, c(1, 12)]
    cs$CollarSerialNumber <- as.character(as.numeric(cs$CollarSerialNumber))
    ntab <- merge(ntab, cs, by.x = "Serial.Number", by.y = "CollarSerialNumber", 
                  all.x = T)
    colnames(ntab)[13:15] <- c("EventEasting", "EventNorthing", 
                               "CurrentStatus")
    locs <- outtra
    locs$CollarSerialNumber <- as.character(as.numeric(as.character(locs$CollarSerialNumber)))
    locs <- locs[which(!(locs$CollarSerialNumber %in% mlist)), 
                 ]
    uni <- unique(locs$CollarSerialNumber)
    tl <- data.frame()
    for (i in 1:length(uni)) {
      vsub <- locs[which(locs$CollarSerialNumber == uni[i]), 
                   ]
      vsub <- vsub[which(vsub$TelemDate == max(vsub$TelemDate, 
                                               na.rm = T)), ]
      tl <- rbind(tl, vsub)
    }
    tl$CollarSerialNumber <- as.numeric(tl$CollarSerialNumber)
    tl <- tl[, c(1, 2, 6, 7)]
    ntab <- merge(ntab, tl, by.x = "Serial.Number", by.y = "CollarSerialNumber")
    colnames(ntab)[16:18] <- c("LatestTelemDate", "LatestEasting", 
                               "LatestNorthing")
    nt1 <- ntab
    nt2 <- ntab
    coordinates(nt1) <- ~EventEasting + EventNorthing
    proj4string(nt1) <- "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
    coordinates(nt2) <- ~LatestEasting + LatestNorthing
    proj4string(nt2) <- "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
    library(rgeos)
    dists <- vector()
    for (p in 1:length(nt1)) {
      j <- nt1[p, ]
      k <- nt2[p, ]
      dist <- gDistance(j, k)
      dists <- c(dists, dist)
    }
    ntab$DistanceFromPreviousEvent <- dists
  }
  if (spp == "elk") {
    vi <- vi[which(as.character(as.numeric(vi$CollarSerialNumber)) %in% 
                     vhist$Serial.Number), ]
    cursas <- vi
    cs <- data.frame()
    uni <- unique(cursas$CollarSerialNumber)
    for (i in 1:length(uni)) {
      s <- cursas[which(cursas$CollarSerialNumber == uni[i]), 
                  ]
      s <- s[which(s$Date == max(s$Date)), ]
      cs <- rbind(cs, s)
    }
    ntab <- vhist
    ntab$Mortality <- NA
    ntab <- ntab[which(!(ntab$Serial.Number %in% mlist)), 
                 ]
    vidat <- vi
    vidat$CollarSerialNumber <- as.character(as.numeric(as.character(vidat$CollarSerialNumber)))
    uni <- unique(vidat$CollarSerialNumber)
  }
  return(ntab)
}
