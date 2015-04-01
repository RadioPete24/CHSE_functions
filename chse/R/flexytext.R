#####################
# FLEXY TEXT READER #
#####################

#############
# read.docx #
#############

require(XML)
read.docx <- function(file, skip=0, output = "vector"){
 
  if(!file.exists(file)){stop("\nspecified file does not exist\nverify file path")}
  
  if(!grepl(".docx$",file)){stop("incorrect file type")}
  
  if(!(output %in% c("vector","knitr"))){stop("output type is not valid")}
  
  tmp <- tempfile()
  if (!dir.create(tmp))
    stop("Temporary directory could not be established.")
  unzip(file, exdir = tmp)  # Unzip to temporary directory
  xmlfile <- file.path(tmp, "word", "document.xml")  # Path to xml document
  doc     <- XML::xmlTreeParse(xmlfile, useInternalNodes=TRUE)  # Import XML
  unlink(tmp, recursive = TRUE)  # Delete unzipped files; no longer needed
  nodeSet <- XML::getNodeSet(doc, "//w:p")  # Access all p-nodes in document
  pvalues <- sapply(nodeSet, XML::xmlValue)  # Return their (textual) values
  if (skip > 0) {pvalues <- pvalues[-seq(skip)]}  # Ignore this many lines
  
  if(output == "vector"){ result <- pvalues } #output as vector
  if(output == "knitr") { result <- paste(pvalues,collapse="  \n") } #output for knitr
  return(result)
  
}

############
# read.txt #
############

read.txt <- function(file, skip=0, output = "vector"){

if(!file.exists(file)){stop("\nspecified file does not exist\nverify file path")}

if(!grepl(".txt$",file)){stop("incorrect file type")}

if(!(output %in% c("vector","knitr"))){stop("output type is not valid")} #Check output type

txttemp <- readChar(file, nchars = file.info(file)$size) #Read file

txttemp2 <- strsplit(txttemp,split="\r\n")[[1]] #Split into lines

if (skip > 0) {txttemp2 <- txttemp2[-seq(skip)]}  # Ignore this many lines

if(output == "vector"){ result <- txttemp2 } #output as vector
if(output == "knitr") { result <- paste(txttemp2,collapse="  \n") } #output for knitr
return(result)

}

#############
# flexytext #
#############

flexytext <- function(file, skip=0, output = "vector"){
  
  if(!grepl(".docx$",file) & !grepl(".txt$",file)){
    stop("\ninvalid file type\nflexytext only accepts .docx and .txt files")
  }
  
  if(grepl(".docx$",file)){
    result <- read.docx(file,skip,output)
    return(result)
  }
  
  if(grepl(".txt$",file)){
    result <- read.txt(file,skip,output)
    return(result)
  }
    
}