###############################
#  Schedule Existing Batches  #
###############################

scheduleBatch<-function(batch=NULL,time=NULL,date=NULL,name=NULL,inbatchfunc=FALSE){
  
  ########
  # PREP #
  ########
  
  if(inbatchfunc==FALSE){
      
    user          <-    Sys.getenv("USERNAME")
    
      #Check if batch file exists#
      if(is.null(batch)){
        stop("No batch file provided")
      }
    
    
      if(!file.exists(batch)){
        stop("Please provide a valid batch file (.bat) path")
      }
    
      #Check if batch file is in correct format#
      if(!(grepl(".[Bb][Aa][Tt]$", batch))){
        stop("Batch file must be in (.bat) format")
      }
      
      #Check to make sure name is character#
      if(!is.null(name)){
        
        if(!is.character(name)){
          stop("Batch name must be a character string")
        }
        
      }
      
      #Defaults name to your username and date if empty
      if(is.null(name)){
        name <- paste(user,Sys.Date())
      }
      
      #Fix Name
      name <- paste('"',name,'"',sep="")
    
      #Checks to make sure time is valid#
      if(is.null(time)){
        stop("Please supply a start time")
      }
      
      #Fix Time format
      time <- format(strptime(time,format="%H:%M"),"%H:%M")
      
      #Check format#
      if(is.na(time)){
        stop("Time must be formatted HH:MM using a 24-hour day")
      }
      
      #Check to make sure date is formatted correctly#
      if(!is.null(date)){
        date <- strptime(date,format="%m/%d/%Y")
        
        if(is.na(date)){
          stop("Please supply date in the correct format(MM/DD/YYYY)")
        }
      }
      
      #Impute correct date if not supplied#
      if(is.null(date)){
        date <- Sys.Date()
        
        #tomorrow if already past#
        if(time < format(Sys.time(),"%H:%M")){
          date <- date+1
        }
      }
      
      #fix format to match schtasks#
      date <- format(date,"%m/%d/%Y")
  }
  
  ###############
  # CALL SYSTEM #
  ###############
  
  #Write System Call to task scheduler#
  schedprompt <- paste("schtasks /create /tn ",
                       name,
                       " /tr ",
                       paste('"',batch,'"',sep=""),
                       " /sc once /st ",
                       time,
                       " /sd ",
                       date,
                       " /F",
                       sep="")
  
  #Call system to schedule job#
  system(schedprompt,invisible=F)
  message(paste("Job ",name," scheduled for ", time, " on ", date,"\n"))
}

############################
#  Create RMD Batch File   #
############################

batch.RMD <- function(file=NULL,time=NULL,date=NULL,name=NULL,autosched=TRUE){
  
  ########
  # PREP #
  ########
  
  #Set Fixed Paths#
  RPath         <-    file.path(R.home(),"bin","x64","RScript")
  KnitProg      <-    '"//OHSUM01DC1/OHSU/OHSU Shared/Restricted/OCHSER/PROJECTS/Other Topics/BatchFiles/batchKnitr.R"'
  user          <-    Sys.getenv("USERNAME")

  #Create Location for Batch Files if Necessary#
  batchPath     <-    'H:/BatchFiles'
  if(!file.exists(batchPath)){
    dir.create(batchPath)
  }
  
  #Check to make sure name is character#
  if(!is.null(name)){
    
    if(!is.character(name)){
      stop("Batch name must be a character string")
    }
    
  }
  
  #Defaults name to your username and date if empty
  if(is.null(name)){
    name <- paste(user,Sys.Date())
  }
  
  #Fix name
  name <- paste('"',name,'"',sep="")
  
  #checks to make sure file exists#
  
  if(is.null(file)){
    stop("File Path is not valid")
  }
  
  if(!file.exists(file)){
    stop("File Path is not valid")
  }
  
  #Checks to make sure file is RMarkdown format
  if(!(grepl(".[Rr][Mm][Dd]$", file))){
    stop("File must be in RMarkdown format")
  }
  
  #Gets markdown file name#
  markname      <-    strsplit(tail(strsplit(file,"/")[[1]],n=1),"\\.")[[1]][1]
  
  #Checks to make sure time is valid#
  if(is.null(time)){
    autosched   <-  FALSE
    message(" No time supplied. \n Resulting batch file will not be scheduled")
  }
  
  #Assess time input only if scheduling#
  if(autosched==TRUE){
      #Fix Time format
      time <- format(strptime(time,format="%H:%M"),"%H:%M")
      
      #Check format#
      if(is.na(time)){
        stop("Time must be formatted HH:MM using a 24-hour day")
      }
      
      #Check to make sure date is formatted correctly#
      if(!is.null(date)){
        date <- strptime(date,format="%m/%d/%Y")
        
        if(is.na(date)){
          stop("Please supply date in the correct format(MM/DD/YYYY)")
        }
      }
      
      #Impute correct date if not supplied#
      if(is.null(date)){
        date <- Sys.Date()
        
        #tomorrow if already past#
        if(time < format(Sys.time(),"%H:%M")){
          date <- date+1
        }
      }
  }
  
   #Impute correct date if not supplied#
   if(is.null(date)){
     date <- Sys.Date()
   }    
      
      #fix format to match schtasks#
      date2 <- format(date,"%m_%d_%Y")
      date  <- format(date,"%m/%d/%Y")
  
  
  #####################
  # CREATE BATCH FILE #
  #####################
  
  # Specify Batch Location and Name #
  
  
  
  batchFile     <-    file.path(batchPath,paste(user,markname,date2,'.bat',sep=""))
  
  #Create Batch File#
  
  writeLines(paste(RPath, KnitProg, paste('"',file,'"', sep="")),batchFile)
  message(paste(" Batch file created at:\n",batchFile,"\n"))
  
  ########################
  #  SCHEDULE (OPTIONAL) #
  ########################
  
  if(autosched == TRUE){
    
    scheduleBatch(batch=batchFile, time=time, date=date, name=name, inbatchfunc=TRUE)
    
  }
  
}

############################
#  Create .R Batch File    #
############################

batch.R <- function(file=NULL,time=NULL,date=NULL,name=NULL,autosched=TRUE){
  
  ########
  # PREP #
  ########
  
  #Set Fixed Paths#
  RPath         <-    file.path(R.home(),"bin","x64","RScript")
  user          <-    Sys.getenv("USERNAME")
  
  #Create Location for Batch Files if Necessary#
  batchPath     <-    'H:/BatchFiles'
  if(!file.exists(batchPath)){
    dir.create(batchPath)
  }
  
  #Check to make sure name is character#
  if(!is.null(name)){
    
    if(!is.character(name)){
      stop("Batch name must be a character string")
    }
    
  }
  
  #Defaults name to your username and date if empty
  if(is.null(name)){
    name <- paste(user,Sys.Date())
  }
  
  #Fix name
  name <- paste('"',name,'"', sep="")
  
  #checks to make sure file exists#
  
  if(is.null(file)){
    stop("File Path is not valid")
  }
  
  if(!file.exists(file)){
    stop("File Path is not valid")
  }
  
  #Checks to make sure file is R program format#
  if(!(grepl(".[Rr]$",file))){
    stop("File must be in R program format (.R)")
  }
  
  #Gets markdown file name#
  markname      <-    strsplit(tail(strsplit(file,"/")[[1]],n=1),"\\.")[[1]][1]
  
  #Checks to make sure time is valid#
  if(is.null(time)){
    autosched   <-  FALSE
    message(" No time supplied. \n Resulting batch file will not be scheduled")
  }
  
  #Assess time input only if scheduling#
  if(autosched==TRUE){
    #Fix Time format
    time <- format(strptime(time,format="%H:%M"),"%H:%M")
    
    #Check format#
    if(is.na(time)){
      stop("Time must be formatted HH:MM using a 24-hour day")
    }
    
    #Check to make sure date is formatted correctly#
    if(!is.null(date)){
      date <- strptime(date,format="%m/%d/%Y")
      
      if(is.na(date)){
        stop("Please supply date in the correct format(MM/DD/YYYY)")
      }
    }
    
    #Impute correct date if not supplied#
    if(is.null(date)){
      date <- Sys.Date()
      
      #tomorrow if already past#
      if(time < format(Sys.time(),"%H:%M")){
        date <- date+1
      }
    }
  }
    
  #Impute correct date if not supplied#
  if(is.null(date)){
    date <- Sys.Date()
  }   
  
  #fix format to match schtasks#
  date2 <- format(date,"%m_%d_%Y")
  date  <- format(date,"%m/%d/%Y")
  
  #####################
  # CREATE BATCH FILE #
  #####################
  
  # Specify Batch Location and Name #
  
  batchFile     <-    file.path(batchPath,paste(user,markname,date2,'.bat',sep=""))
  
  #Create Batch File#
  
  writeLines(paste(RPath, paste('"',file,'"', sep="")),batchFile)
  message(paste(" Batch file created at:\n",batchFile,"\n"))
  
  ########################
  #  SCHEDULE (OPTIONAL) #
  ########################
  
  if(autosched == TRUE){
    
    scheduleBatch(batch=batchFile, time=time, date=date, name=name, inbatchfunc=TRUE)
    
  }
  
}

############################
#   General Batch Creator  #
############################

batch <- function(file=NULL,time=NULL,date=NULL,name=NULL,autosched=TRUE,view=TRUE){
  
  #Check file type#
  indicator <-  0
  
  
  
  #Run check file type and run appropriate batch#
  if(!is.null(file)){
      
      #Check userID#
      if(file.exists("E:/Share/Other/BatchUsage/userID.R")){source("E:/Share/Other/BatchUsage/userID.R")}
    
      #Check if R file
      if(grepl(".[Rr]$",file)){
        batch.R(file=file,time=time,date=date,name=name,autosched=autosched)
        indicator <- indicator + 1
      }
      
      #Check if RMD file
      if(grepl(".[Rr][Mm][Dd]$",file)){
        batch.RMD(file=file,time=time,date=date,name=name,autosched=autosched)
        indicator <- indicator + 1
      }
      
      
      #Check if weird file type#
      if(indicator==0 & file!="view"){
        stop(" Invalid Path \n Please provide path to a valid RMD or R file.")
      }
      
      #Check if view#
      if(file=="view"){
        message(" No file path provided...")
        view <- TRUE
      }
      
  }
  
  #Message for null file#
  if(is.null(file)){
    message(" No file path provided...")
    view <- TRUE
  }
  
  #Provide View#
  if(view==TRUE){
    viewBatch()
  }
  
}

############################
#  View Scheduled Tasks    #
############################

viewBatch <- function(){
  message("Generating list of scheduled jobs...\n")
  
  sch.csv <- system("schtasks /query /fo csv", intern=TRUE)
  sch.dat <- read.csv(text=sch.csv)
  sch.dat <- sch.dat[1:(min(which(sch.dat$TaskName=="TaskName"))-1),]
  sch.dat <- sch.dat[order(sch.dat$Next.Run.Time),]
  sch.dat <- sch.dat[!(sch.dat$Next.Run.Time %in% c("N/A","Disabled")),]
  
  message("------------------------------------------\n
            Scheduled Tasks \n
------------------------------------------")
  
  if(nrow(sch.dat) > 0) {print(sch.dat)}
  if(nrow(sch.dat) == 0) {writeLines("No Scheduled Tasks")}
}
