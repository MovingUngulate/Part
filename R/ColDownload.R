#' @title Internet Collar Download
#
#' @description This function downloads Iridium GPS data from ATS servers. Internet connection
#' must be available and ATS servers online. User must have valid ATS Iridium account. In addition
#' mortality/battery warnings and transmissions are also downloaded and returned.
#' @param username Username for Iridium Account (not stored) as character vector
#' @param password Password for Iridium Account (not stored) as character vector
#' @param dirdown Directory to download the file to
#' @param cType Type of collar/system (currently only supports 'ATS/IRID' and 'ATS/GSTAR')
#' @return Resulting object is a list of Two elements. First element is a SpatialPointsDataFrame of
#' all the GPS data on the account, the second is a dataframe of all transmissions received by
#' the satellite
#'
#' \strong{Spatial Data Column Description:}
#' \item{CollarSerialNumber}{ATS Designated Collar Serial Number}
#' \item{TelemDate}{POSIX Field of collare fixes in the USER'S timezone. NOTE: may need to alter timezone}
#' \item{HDOP}{Horizontal Dilution of Precision}
#' \item{NumSats}{Number of satellites used for GPS Fix}
#' \item{2D/3D}{Whether fix is a 2d or 3d fix. Values are either 2 or 3}
#' @keywords Iridium, iridium
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#' \donttest{ColDownload(username='yourusername',password='yourpassword',dirdown='/home/mhayes1/Desktop/Testing/',cType='ATS/IRID')}
#'
ColDownload<-function(username="",password="",dirdown="",cType='ATS/IRID'){

  if(cType=='ATS/IRID'){

        
    #start session
    s<-rvest::html_session('https://www.atsidaq.net/login.aspx')

    #Pull forms on the page
    f1<-rvest::html_form(s)

    #Set username and password
    f1[[1]][[5]][[4]]$value<-username
    f1[[1]][[5]][[5]]$value<-password

    
    p<-as.character('login')
    attempt <- 0
    while( grepl('login',as.character(p)) && attempt <= 15 ) {
      attempt <- attempt + 1
      
      try({
    #submit the form
    p2<-rvest::submit_form(s,f1[[1]],'btt_SignIn')

    p<-as.character(p2$handle$url)
      })
      
    }
    #navigate to next page
    jp<-rvest::jump_to(p2,'https://www.atsidaq.net/default.aspx')




    ab<-xml2::read_html(jp)

    nams<- ab %>%
      rvest::html_nodes("span")%>%
      rvest::html_text()

    a<-NA

    for(i in 1:length(nams)){
      if(nchar(nams[i])==6){
        a<-i
        break
      }else{
        next
      }
    }

    nams<-nams[(a-1):length(nams)]


    totl<-length(nams)/11

    #nams<-nams[9:length(nams)]


    #totl<-length(nams)/11

    statdf<-data.frame()


    n1<-seq(2,length(nams),11)
    n2<-seq(8,length(nams),11)
    n3<-seq(9,length(nams),11)
    n4<-seq(11,length(nams),11)
    for(i in 1:totl){




      int<-data.frame(Serial=nams[n1[i]],Active=nams[n2[i]],
                      Mort=nams[n3[i]],Battery=nams[n4[i]],stringsAsFactors = F)

      statdf<-rbind(statdf,int)
    }


    #Pull forms on the page
    f2<-rvest::html_form(jp)

    d1P<-paste(dirdown,'AllDown.txt',sep='')
    #download data
    gf<-rvest::submit_form(jp,f2[[1]],'ctl00$ContentPlaceHolder1$DownloadAll3',httr::write_disk(d1P,overwrite=T))
    #read in data
    d1<-read.table(d1P,stringsAsFactors = F,sep=',',header=T)

    #data munging
    d1$CollarSerialNumber<-as.character(d1$CollarSerialNumber)
    d1$Hour <- sprintf(paste("%02d"), d1$Hour)
    d1$Minute <- sprintf(paste("%02d"), d1$Minute)
    d1$Date<-as.POSIXct(paste(d1$Julianday,d1$Year,sep='/'), '%j/%y',tz='MST')
    d1$time <- paste(d1$Hour, d1$Minute, d1$Sec, sep=":")
    d1$dt <- paste(as.character(d1$Date), d1$time, sep=" ")
    d1$TelemDate <- as.POSIXct(strptime(d1$dt, format="%Y-%m-%d %H:%M"), tz="MST")

    d1<-d1[,c(1,17,8,9,10,11,13,6,7)]

    d1<-d1[complete.cases(d1),]

    sp::coordinates(d1)<-~Longitude+Latitude
    sp::proj4string(d1)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'


    #download the transmission data
    d1P<-paste(dirdown,'TransMissDown.txt',sep='')
    gf<-rvest::submit_form(jp,f2[[1]],'ctl00$ContentPlaceHolder1$DownloadAll4',httr::write_disk(d1P,overwrite=T))

    d2<-read.table(d1P,stringsAsFactors = F,sep=',',fill=T,
                   col.names=paste('column',1:16,sep='_'))

    d2<-d2[-1,-16]
    names(d2)<-c('Serial','TelemDate','NumFixes','BattVoltage','Mortality','BreakOff','GPSOnTime','SatOnTime',
                 'SatErrors','GMTOffset','LowBatt','Event1','Event2','Event3','Event4')


    dd<-d2[,1:11]


    de<-d2[,c(1,2,12:15)]

    de$All<-paste(de$Event1,de$Event2,de$Event3,de$Event4,sep=':')

    fixfun <- function(x) {
      outp <- "No"
      if (grepl("Unknown-No Sync:::", x["All"]) == TRUE) {
        outp <- "Unknown/No Sync"
        return(outp)
      }
      if (grepl("Birth-Not yet triggered:::", x["All"]) == 
          TRUE) {
        outp <- "Birth Not yet triggered"
        return(outp)
      }
      if (grepl("Birth-triggered by temperature:::", x["All"]) == 
          TRUE) {
        outp <- "Birth triggered by temperature"
        return(outp)
      }
      if (grepl("Birth-triggered by lack of comm:::", x["All"]) == 
          TRUE) {
        outp <- "Birth triggered by lack of comm"
        return(outp)
      }
      if (grepl("Fawn0-Comm:::", x["All"]) == TRUE) {
        outp <- "Fawn0-Comm"
        return(outp)
      }
      if (grepl("Birth-triggered by light and temperature:::", 
                x["All"]) == TRUE) {
        outp <- "Birth triggered by light and temperature"
        return(outp)
      }
      if (grepl("Birth-triggered by light:::", x["All"]) == 
          TRUE) {
        outp <- "Birth triggered by light"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1-Comm:Comm:", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Comm:Fawn1-Comm"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Comm:Absence", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Comm:Fawn1-Absence"
        return(outp)
      }
      if (grepl(":::", x["All"]) == TRUE) {
        outp <- "No Status"
        return(outp)
      }
      if (grepl("None:::", x["All"]) == TRUE) {
        outp <- "No Status"
        return(outp)
      }
      if (grepl("Fawn0-Absence:::", x["All"]) == TRUE) {
        outp <- "Fawn0-Absence"
        return(outp)
      }
      if (grepl("Fawn0-Mortality:::", x["All"]) == TRUE) {
        outp <- "Fawn0-Mortality"
        return(outp)
      }
      if (grepl("Fawn0:None:None-Absence:None", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Absence"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1-Comm:Not yet triggered:", 
                x["All"]) == TRUE) {
        outp <- "Fawn0-Comm:Fawn1-Not Yet Triggered"
        return(outp)
      }
      if (grepl("Fawn0:None:None-Comm:None", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Comm"
        return(outp)
      }
      if (grepl("Fawn0:None-Absence:None:", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Absence"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Absence:Absence", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Absence:Fawn1-Absence"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1-Comm:Mortality:", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Comm:Fawn1-Mortality"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1-Absence:Comm:", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Absence:Fawn1-Comm"
        return(outp)
      }
      if (grepl("None:None:None-None:None", x["All"]) == 
          TRUE) {
        outp <- "No Status"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1-Mortality:Comm:", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Mortality:Fawn1-Comm"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1-Absence:Mortality:", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Absence:Fawn1-Mortality"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1-Mortality:Mortality:", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Mortality:Fawn1-Mortality"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Absence:Comm", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Absence:Fawn1-Comm"
        return(outp)
      }
      if (grepl("Fawn0:None:None-Mortality:None", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Mortality"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1-Mortality:Not yet triggered:", 
                x["All"]) == TRUE) {
        outp <- "Fawn0-Mortality:Fawn1-Not yet triggered"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Comm:Comm", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Comm:Fawn1-Comm"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Mortality:Absence", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Mortality:Fawn1-Absence"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Absence:Mortality", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Absence:Fawn1-Mortality"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Mortality:Mortality", 
                x["All"]) == TRUE) {
        outp <- "Fawn0-Mortality:Fawn1-Mortality"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Comm:Not yet triggered", 
                x["All"]) == TRUE) {
        outp <- "Fawn0-Comm:Fawn1-Not yet triggered"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Mortality:Comm", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Mortality:Fawn1-Comm"
        return(outp)
      }
      if (grepl("None:Fawn1:None-None:Not yet triggered", 
                x["All"]) == TRUE) {
        outp <- "Fawn1-Not yet triggered"
        return(outp)
      }
      if (grepl("Fawn0:Fawn1:None-Comm:Mortality", x["All"]) == 
          TRUE) {
        outp <- "Fawn0-Comm:Fawn1-Mortality"
        return(outp)
      }
      if (nchar(x["All"]) == 0) {
        outp <- "No Status"
        return(outp)
      }
      if (outp == "No") {
        outp <- "Unknown Status Message"
        return(outp)
      }
      #return(outp)
    }

    de$RealStatus<-apply(de,1,FUN=fixfun)


    de<-de[,c(1,2,8)]

    dd<-cbind(dd,de[,3])

    names(dd)[12]<-'NeoLink Status'

    dd[,12]<-as.character(dd[,12])


    file.remove(paste0(dirdown,'AllDown.txt'))
    file.remove(paste0(dirdown,'TransMissDown.txt'))


    
    #return it all
    return(list(d1,dd,statdf))

  }

  if(cType=='ATS/GSTAR'){

    #start session
    s<-rvest::html_session('https://www.atsdat.net/login.aspx')

    #Pull forms on the page
    f1<-rvest::html_form(s)

    #Set username and password
    f1[[1]][[5]][[4]]$value<-username
    f1[[1]][[5]][[5]]$value<-password

    p<-as.character('login')
    attempt <- 0
    while( grepl('login',as.character(p)) && attempt <= 15 ) {
      attempt <- attempt + 1
      
      try({
        #submit the form
        p2<-rvest::submit_form(s,f1[[1]],'btt_SignIn')
        
        p<-as.character(p2$handle$url)
      })
    }
    #navigate to next page
    jp<-rvest::jump_to(p2,'https://www.atsdat.net/default.aspx')



    ab<-xml2::read_html(jp)

    nams<- ab %>%
      rvest::html_nodes("span")%>%
      rvest::html_text()

    a<-NA

    for(i in 1:length(nams)){
      if(nchar(nams[i])==6){
        a<-i
        break
      }else{
        next
      }
    }

    nams<-nams[(a-1):length(nams)]


    totl<-length(nams)/11

    statdf<-data.frame()


    n1<-seq(2,length(nams),11)
    n2<-seq(7,length(nams),11)
    n3<-seq(9,length(nams),11)
    #n4<-seq(11,length(nams),11)
    for(i in 1:totl){




      int<-data.frame(Serial=nams[n1[i]],
                      Mort=nams[n2[i]],Battery=nams[n3[i]],stringsAsFactors = F)

      statdf<-rbind(statdf,int)
    }


    #Pull forms on the page
    f2<-rvest::html_form(jp)

    d1P<-paste(dirdown,'AllDown.txt',sep='')
    #download data
    gf<-rvest::submit_form(jp,f2[[1]],'ctl00$ContentPlaceHolder1$DownloadAll3',httr::write_disk(d1P,overwrite=T))
    #read in data
    d1<-rvest::read.table(d1P,stringsAsFactors = F,sep=',',header=T)

    #data munging
    d1$CollarSerialNumber<-as.character(d1$CollarSerialNumber)
    d1$Hour <- sprintf(paste("%02d"), d1$Hour)
    #d1$Minute <- sprintf(paste("%02d"), d1$Minute)
    d1$Date<-as.POSIXct(paste(d1$Julianday,d1$Year,sep='/'), '%j/%y',tz='MST')
    d1$time <- paste(d1$Hour, d1$Minute, d1$Sec, sep=":")
    d1$dt <- paste(as.character(d1$Date), d1$time, sep=" ")
    d1$TelemDate <- as.POSIXct(strptime(d1$dt, format="%Y-%m-%d %H"), tz="MST")

    d1<-d1[,c(1,14,5,6,7,8,10)]

    d1<-d1[complete.cases(d1),]

    sp::coordinates(d1)<-~Longitude+Latitude
    sp::proj4string(d1)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'


    #download the transmission data
    d1P<-paste(dirdown,'TransMissDown.txt',sep='')
    gf<-rvest::submit_form(jp,f2[[1]],'ctl00$ContentPlaceHolder1$DownloadAll4',httr::write_disk(d1P,overwrite=T))

    d2<-read.table(d1P,stringsAsFactors = F,sep=',',header=T)

    file.remove(paste0(dirdown,'AllDown.txt'))
    file.remove(paste0(dirdown,'TransMissDown.txt'))


    
    #return it all
    return(list(d1,d2,statdf))

  }


  if(cType=='LOTEK/IRID'){
      s<-rvest::html_session("https://webservice.lotek.com/default.aspx")

      f1<-rvest::html_form(s)



      #set_values(f1, txt_username='KE13307MO',txt_password='P3fP7V$R')

      f1[[1]][[5]][[4]]$value<-username
      f1[[1]][[5]][[5]]$value<-password

      p2<-rvest::submit_form(s,f1[[1]],'ctl00$LeftBarContent$lvLeftPanel$LoginGWS$LoginButton')

      jp<-rvest::follow_link(p2,i='List View & Download')
      #c<-html_session(dataURL)
      d<-rvest::html_form(jp)[[1]]

      #httr::POST()
      #d$fields[[20]]$value<-'Text Listing'
      #d$fields[[11]]$value<-'checked'

      d1P<-paste(dirdown,'LotDown.txt',sep='')
      #d$fields[[4]]$type<-'text'
      #d$fields[[4]]$value<-"{\"isEnabled\":true,\"logEntries\":[],\"selectedIndices\":[0,1],\"checkedIndices\":[],\"scrollPosition\":0}"
      d$fields[[4]]$value<-"{\"isEnabled\":true,\"logEntries\":[],\"selectedIndices\":[0,1],\"checkedIndices\":[],\"scrollPosition\":0}"

      d$fields[[8]]<-d$fields[[22]]
      ff<-rvest::submit_form(jp,d,submit='ctl00$LeftBarContent$lvLeftPanel$btnDownload',httr::write_disk(d1P,overwrite=T),encode='multipart')


      return(NULL)


}

}


