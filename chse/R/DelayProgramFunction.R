DelayProgram <- function(seconds=NULL,minutes=NULL,hours=NULL,date=NULL,time=NULL){
  
  #Check arguments
  timer  <- !is.null(seconds) | !is.null(minutes) | !is.null(hours)
  schedt <- !is.null(time)
  schedd <- !is.null(date)
  sched  <- schedt==T | schedd==T
  
  if(timer==T & sched==T){
    stop("Function only supports the use of hours/minutes/seconds or date/time, not both simultaneously")
  }   #Error for double specifying
  if(schedd==T & schedt==F){
    stop("Use of date argument requires time argument")
  } #Error for date without time
  
  
  #Default wait time is 0 seconds
  dTime <- 0
  
  if(!is.null(time)){
    
    if(!is.null(date)){
      date <- strptime(date, format = "%m-%d-%Y")
      
      if(is.na(date)){
        stop("Date must formatted properly (MM-DD-YYYY) using USA standard format")
      } #Error for improper date formate
      
    } #Converts date input into a real date
    
    if(is.null(date)){
      date <- Sys.Date()
      
      if(strptime(time,format="%H:%M") < Sys.time()){
        date = date + 1
      } #If time has past, impute next day instead
      
    } #If date is missing, impute current date
    
    #Combine Date and time and calculate out difference from current Sys.time()
    dattime <-  strptime(paste(date,time), format = "%Y-%m-%d %H:%M")
    ttime   <-  difftime(dattime,Sys.time(),units="secs")
    
    if(is.na(ttime)){
      stop("Time must be formatted properly (HH:MM) using military(24 hour) time")
    } #Error for incorrect time format
    
    #Calculate hours/minutes seconds from time difference
    hours   <-  as.numeric(floor(ttime/3600))
    minutes <-  as.numeric(floor((ttime - hours*3600)/60))
    seconds <-  as.numeric(floor(ttime-hours*3600-minutes*60))
    
  } #Converts Date/Time input into hours/minutes/seconds
  
  if(!is.null(hours)){
    
    if(!is.numeric(hours)){
      stop("Hours must be numeric!")
    } #Error for incorrect hours format
    
    dTime <- dTime + hours*3600
    
  } #Adds hours onto delay time
  
  if(!is.null(minutes)){
    
    if(!is.numeric(minutes)){
      stop("Minutes must be numeric!")
    } #Error for incorrect minutes format
    
    dTime <- dTime + minutes*60
    
  } #Adds minutes onto delay time
  
  if(!is.null(seconds)){
    
    if(!is.numeric(seconds)){
      stop("Seconds must be numeric!")
    } #Error for incorrect seconds format
    
    dTime <- dTime + seconds
    
  } #Adds seconds onto delay time
  
  if(dTime < 0){
    stop("Please specify a time in the future. \n
         ############################## \n
         # This isn't a time machine! # \n
         ##############################")
  } #Error for selecting a negative delay time
  
  #Calculate out true sec/min/hour/day/year for delay end
  msec  <- dTime %% 60
  mmin  <- floor(dTime/60) %% 60
  mhour <- floor(dTime/(60*60)) %% 24
  mday  <- floor(dTime/(60*60*24)) %% 365
  myear <- floor(dTime/(60*60*24*365))
  
  #Send messages with delay information
  words <- paste("Waiting for ",
                 ifelse(myear>0, paste(myear, "Years "  ), ""),
                 ifelse(mday>0,  paste(mday,  "Days "   ), ""),
                 ifelse(mhour>0, paste(mhour, "Hours "  ), ""),
                 ifelse(mmin>0,  paste(mmin,  "Minutes "), ""),
                 ifelse(msec>0,  paste(msec,  "Seconds "), ""),
                 "(",
                 dTime,
                 "s).",
                 sep=""
  )
  
  
  if(!is.null(seconds)){if(seconds==8675309){message("Jenny, don't change your number...")}}
  words2 <- paste("Will resume at ", Sys.time()+dTime,".", sep="")
  message(words)
  message(words2)
  
  t1 <- Sys.time()
  
  
  #Delay Mechanism#
  Sys.sleep(dTime)
  
  #True delay message
  t2 <- Sys.time()
  
  message(paste("True delay time:", round(t2-t1,1), "seconds."))
  
}