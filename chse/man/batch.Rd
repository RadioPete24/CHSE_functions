\name{batch}
\alias{batch}
\alias{batch.R}
\alias{batch.RMD}
\alias{scheduleBatch}
\alias{viewBatch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Windows Batching and Scheduling Program
}
\description{
Creates batch files and schedules batch jobs for specific time dates in Windows Task Scheduler.
}
\usage{
batch(file=NULL,time=NULL,date=NULL,name=NULL,autosched=TRUE,view=TRUE) \cr
batch() \cr
scheduleBatch(batch=NULL,time=NULL,date=NULL,name=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The file path for the R program.  Accepts both RMarkdown files (.RMD) and standard R program files (.R). Will only run schedule view if omitted.}
  \item{time}{The desired program start time.  Formatted as a character string, "HH:MM".  This input is required if scheduling}
  \item{date}{The disired program start date.  Formatted as a character string, "MM/DD/YYYY". This input is not required.  Will default to the date with the next occurance of \code{time} if not provided.}
  \item{name}{An optional name to identify the batch job once scheduled. This should be specific enough to differentiate the job from similar jobs. Will default to the current user name and date if left blank.  Formatted as a character string. Only required if scheduling a job}
  \item{autosched}{Tells batch whether to automatically schedule the job.  If set to FALSE, then batch will stop after creating the batch file.  Batch files can then be scheduled manually or by using the scheduleBatch function.}
  \item{view}{Tells batch whether print the list of current scheduled jobs.}
  \item{batch}{The file path for the batch file (.bat).}
}
\details{
\code{batch} creates a batch file that automates running an R program or knitting an RMarkdown file. Programs will run on the same version of R that is being used to schedule. On default, \code{batch} will also make a call to Windows Task Scheduler to schedule the batch file to run the specified time and date.  \code{batch} will also display a list of the currently scheduled jobs.
\cr \cr
Running \code{batch()} without any inputs will only display currently scheduled jobs.
\cr \cr
\code{scheduleBatch} calls Windows Task Scheduler to schedule a specified batch file.  Can be useful for running preexisting batch files.
}

\author{
T.Meath
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\href{http://www.digdang.com/media/images/sombrero_chinchilla_13404.jpg}{chinchilla}}
}
\examples{

batch()
#Other examples don't play well with the package compiler#

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~batch }
\keyword{ ~schedule }
