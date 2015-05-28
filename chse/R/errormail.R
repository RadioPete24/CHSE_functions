errormail <- function(to){

  options(error=function(){
    
    tpath <- file.path("H:","error.txt")    
    write(geterrmessage(),
          file = tpath)
    
    library(gmailR)
    gmail(to = to,
          password = "rojothellama",
          subject = "test email from server",
          message = "Error attached. EOM",
          attachment = tpath,
          from = "chse.server@gmail.com",
    #    username = "chse.server"
    )
    
  }
    
    
    )
  
}

