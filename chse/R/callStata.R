callStata <- function (f, path=tempdir(), tempPath="E:\\Share\\Temp") {
  #Written by Ben Chan (chanb@ohsu.edu)
  
  # Usage:
  # > callStata(do-file-name-as-character-object,
  #             path=path-of-do-file-as-character-object,
  #             tempPath=temp-folder-path-as-character-object)
  # 
  # A log file is created in the same folder as the .do file
  #
  # Because Stata is stupid, dynamically create a temporary batch file.
  # The batch file contains 3 lines
  #     1. Set the path for temporary files (defaults to large, shared server storage)
  #     2. Change the directory where the .do file is located; this will also set where the .log file gets writting
  #     3. Execute StataMP using the .do file
  # Execute this batch file using a system() function call.
  #
  # To do:
  # 1. Scan the log file for error messages and display a warning
  if (grepl(".do$", f)) {
    settemp <- sprintf("set STATATMP=%s", tempPath)
    cd <- sprintf("cd \"%s\"", path)
    exeFile <- "E:\\Share\\Applications\\Stata13\\StataMP-64.exe"
    doFile <- sprintf("%s\\%s", path, f)
    logFile <- sprintf("%s\\%s", path, gsub(".do$", ".log", f))
    cmd <- sprintf("\"%s\" /e \"%s\"",
                   exeFile,
                   doFile)
    batfile <- file.path(tempdir(), "stata.bat")
    cat(c(settemp, cd, cmd), file=batfile, sep="\n")
    system(batfile, invisible=FALSE)
    cat(readLines(doFile), sep="\n")
    message(sprintf("See %s for the log.", logFile))
    #     if (length(grep("^ERROR:", readLines(logFile)) > 0)) {
    #       warning(sprintf("\nERROR: See line %s in %s.", grep("^ERROR:", readLines(logFile)), logFile))
    #     } else {
    #       message(sprintf("No errors\nSee %s for details.", logFile))
    #     }
  }
}