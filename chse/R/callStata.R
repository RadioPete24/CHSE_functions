callStata <- function (f, path=tempdir()) {
  #Written by Ben Chan (chanb@ohsu.edu)
  
  # Usage:
  # > callStata("auto.do")
  # 
  # A log file is created in the same folder as the .do file
  #
  # To do:
  # 1. Scan the log file for error messages and display a warning
  if (grepl(".do$", f)) {
    exeFile <- "E:\\Share\\Applications\\Stata13\\StataMP-64.exe"
    doFile <- sprintf("%s\\%s", path, f)
    logFile <- sprintf("%s\\%s", path, gsub(".do$", ".log", f))
    cmd <- sprintf("\"%s\" /e \"%s\"",
                   exeFile,
                   doFile)
    system(cmd, invisible=FALSE)
    cat(readLines(doFile), sep="\n")
    #     if (length(grep("^ERROR:", readLines(logFile)) > 0)) {
    #       warning(sprintf("\nERROR: See line %s in %s.", grep("^ERROR:", readLines(logFile)), logFile))
    #     } else {
    #       message(sprintf("No errors\nSee %s for details.", logFile))
    #     }
  }
}