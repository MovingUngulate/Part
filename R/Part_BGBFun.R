#' @title Parturition BGB Creation
#
#' @description Creates BGB variables for predicting parturition timing in ungulates given longitudinal
#' relocation data via parallel processing. Internally used in parturition methodology.
#' @param data "Cleaned" data
#' @param xname column name for x coordinates
#' @param yname column name for y coordinates
#' @param timename column name for posix time
#' @param idname column name for unique individual identifier
#' @param projstring character proj4string
#' @param ncpus Number of cpus to use
#' @return Resulting object is the data with BGB variables attached
#' @keywords part, parturition
#' @export
#' @examples
#' \donttest{dyn_dat <- BGBFun(data = dat, xname = "x", yname = "y", timename = "TelemDate", idname = "UAID", projstring = projection, ncpus = ncpus)}
#'
Part_BGBFun<-function(data,xname,yname,timename,idname,projstring,ncpus){
  data <- as.data.frame(data)
  data <- data[complete.cases(data), ]
  data <- data[order(data[, idname], data[, timename]), ]
  ssub <- move::move(data[, xname], data[, yname], data[, timename],
               proj = sp::CRS(projstring), animal = data[, idname])
  ssub <- sp::spTransform(ssub, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  spdata <- sp::spTransform(ssub, center = T)
  spdata <- move::split(spdata)
  cl <- snow::makeSOCKcluster(rep("localhost", ncpus))
  snow::clusterExport(cl, "spdata", envir = environment())


  snow::clusterEvalQ(cl, library(move))

  # calcate movement statistic, split move stack to consider each trajectory seperately
  dBGBvar <- snow::parLapply(cl=cl,x=spdata, fun=move::dynBGBvariance, margin=21, windowSize=43,
                             locErr=21)
  snow::stopCluster(cl)


  return(dBGBvar)
}
