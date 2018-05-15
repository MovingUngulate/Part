#' @title Email Auto Update
#
#' @description After the parturition update has been created, this function automatically sends the update
#' @param from email address sending from
#' @param to email address sending to
#' @param subject email subject
#' @param SP server and port of outgoing email address
#' @param attachpath full path to attachment for email
#' @param progpath path to sendEmail program
#' @param username username for your email login
#' @param password password for your email login
#' @keywords email, update
#' @export
#' @examples
#' \donttest{sendUpdate(from="mhayes1@uwyo.edu",to='mhayes1@uwyo.edu',
#'                      subject='Test email',SP="smtp.office365.com:587",
#'                      attachpath='C:/Users/mhayes1/Desktop/attach.xls',
#'                      progpath='C:/Users/mhayes1/sendEmail/sendEmail.exe',
#'                      username='mhayes1@uwyo.edu',password='password')}
#'


sendUpdate<-function(from,to,subject="Test Email from R",
                     SP="smtp.office365.com:587",attachpath,
                     progpath='C:/Users/mhayes1/sendEmail/sendEmail.exe',
                     username,password){
  
  # Create the required command line parameters for sendEmail to work
  paramsList <- list()
  
  #this is the from address
  paramsList$fromAddress <- c("-f",from)
  
  #to addresse(s). Multiple addresses would look like: c("-t","mhayes1@uwyo.edu,mhayes2@uwyo.edu")
  paramsList$toAddress <- c("-t",to)
  
  #Email Subject
  paramsList$emailSubject <- c("-u",subject)
  
  #Alter this portion, after paste, with what you want the email to say
  paramsList$listemailMessage <- c("-m",paste("Sent at ",format(Sys.time(),"%Y-%d-%m  %H:%M:%S"),sep=" "))
  
  #This should be correct for UWYO email
  paramsList$serverAndPort <- c("-s",SP)
  
  #If you want to add an attachment, put the file here. This 
  #should be able to handle attachments of all formats, as long as the
  #requisite file is present. As is, if the attachment is not there
  #the entire email will fail to send, could test for presence and attach if it
  #is present. You could write a file out in your script and send it to yourself.
  paramsList$fileAttachPath <- c("-a",attachpath)
  
  #UWYO username with @uwyo.edu
  paramsList$accUsername <- c("-xu",username)
  
  #UWYO password, replace ********* with your actual password
  paramsList$accPassword <- c("-xp",password)
  
  # Combine to create one single function call
  suffixCall <- paste(do.call("c",paramsList),collapse = " ")
  
  #This would be the path where the sendEmail program is at
  commandCall <- paste(progpath,suffixCall,sep = " ")
  
  # Issue the command via system() - sending it to CMD
  returnVal <- system('cmd', input=commandCall,intern=T,wait=T)
  
  #this just prints results and tells you if it sent or not and what happened if not. Debug use only.
  #print(returnVal)
  
}