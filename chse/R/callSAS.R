callSAS <- function (f, path=tempdir(), workDir="E:\\Share\\Temp\\chanb\\SASWork") {
  #Written by Ben Chan (chanb@ohsu.edu)
  
  # Usage:
  # > callSAS("getClaimsMed.sas")
  if (grepl(".sas$", f)) {
    exeFile <- "C:\\Program Files\\SASHome\\SASFoundation\\9.3\\sas.exe"
    sasFile <- sprintf("%s\\%s", path, f)
    logFile <- sprintf("%s\\%s", path, gsub(".sas$", ".log", f))
    cmd <- sprintf("\"%s\" -sysin \"%s\" -log \"%s\" -print \"%s\" -work \"%s\"",
                   exeFile,
                   sasFile,
                   logFile,
                   logFile,
                   workDir)
    system(cmd, invisible=FALSE)
    cat(readLines(sasFile), sep="\n")
    if (length(grep("^ERROR:", readLines(logFile)) > 0)) {
      warning(sprintf("\nERROR: See line %s in %s.", grep("^ERROR:", readLines(logFile)), logFile))
    } else {
      message(sprintf("No errors\nSee %s for details.", logFile))
    }
  }
}