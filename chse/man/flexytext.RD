\name{flexytext}
\alias{flexytext}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Word and Txt Document Reader
}
\description{
Reads text from .docx and .txt files.
}
\usage{
flexytext(file, skip=0, output = "vector")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The file path for the text file.  Accepts both Microsoft Word files (.docx) and plain text files (.txt).}
  \item{skip}{The number of rows to skip when reading the text file.}
  \item{output}{the the type of output to return. Can be set to either \code{"vector"} or \code{"knitr"}.  The vector option will return a character vector with each element representing a different line from the document.  The knitr option will output a single character value containing the source document in markdown notation.  The knitr output can be printed as markdown text using the function: \code{cat(output)} and in a chunk with the chunk option \code{results="asis"}}
}
\details{
\code{flexytext} reads the text content of a \code{.txt} or \code{.docx} file. See the \code{output} specifications for details on output. Currently, \code{flexytext} can only capture line breaks.  Other formatting settings are lost, and will have to be recoded if necessary.  \code{flexytext} was not made to read special characters, and it is likely they will not be read in correctly if included.
}

\author{
T.Meath
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\href{http://zippy.gfycat.com/SadFancyDassie.gif}{penguins}}
}
\examples{

}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~text }
\keyword{ ~read }
