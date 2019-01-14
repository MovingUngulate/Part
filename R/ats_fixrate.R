#' @title Change fix rate on ATS collars
#
#' @description Automates changing fix rates on all collars in an ATS account
#' @param user ATS IDAQ username (character vector)
#' @param pass ATS IDAQ password (character vector)
#' @param fixrate fixrate in hours, one of: (4,6,8,12,24,48,72,96,120,144,168)
#' 
#' @keywords ATS, fix rate, fixrate
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#' \donttest{ats_fixrate(user='username', pass='password', fixrate = 48)}

ats_fixrate<-function(user, pass, fixrate){
  #start session
  s<-rvest::html_session('https://www.atsidaq.net/login.aspx')
  
  #Pull forms on the page
  f1<-rvest::html_form(s)
  
  #Set username and password
  f1[[1]][[5]][[4]]$value<-user
  f1[[1]][[5]][[5]]$value<-pass
  
  
  p<-as.character('login')
  attempt <- 0
  while( grepl('login',as.character(p)) && attempt <= 15 ) {
    attempt <- attempt + 1
    
    try({
      #submit the form
      p2<-rvest::submit_form(s,f1[[1]],'btt_SignIn')
      
      p<-as.character(p2$url)
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
  
  #we only want to mess with active collars cause ATS will email us otherwise.
  statdf$n<-1:nrow(statdf)
  statdf <- statdf[statdf$Active=='ACTIVE',]
  
  #Pull forms on page
  f2<-rvest::html_form(jp)
  
  #create list for an lapply
  ll = as.list(statdf$n)
  
  #set int to the hour interval you want, one of (4,6,8,12,24,48,72,96,120,144,168)
  changeFun<-function(x,int=fixrate){
    subnam = paste0('ctl00$ContentPlaceHolder1$DataList1$ctl',sprintf('%02d',(x-1)),'$Command4')
  
    gf3<-rvest::submit_form(jp,f2[[1]],subnam)
  
    f3<-rvest::html_form(gf3)
  
    if(int == 4){
    g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand04')
    }
    if(int == 6){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand06')
    }
    if(int == 8){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand08')
    }
    if(int == 12){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand12')
    }
    if(int == 24){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand24')
    }
    if(int == 48){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand48')
    }
    if(int == 72){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand72')
    }
    if(int == 96){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand96')
    }
    if(int == 120){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand11')
    }
    if(int == 144){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand22')
    }
    if(int == 168){
      g3<-rvest::submit_form(jp,f3[[1]],'ctl00$ContentPlaceHolder1$commmand33')
    }
  }
  
  system.time({ lapply(ll,changeFun) })
}




