\name{DelayProgram}
\alias{DelayProgram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Delay Program
}
\description{
Pauses a program for a specific period of time or until a specific time and date.
}
\usage{
DelayProgram(seconds=NULL, minutes=NULL, hours=NULL, time=NULL, date=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seconds}{The number of seconds in the delay}
  \item{minutes}{The number of minutes in the delay}
  \item{hours}{The number of hours in the delay}
  \item{time}{The time to resume the program. Formatted as a character string, "HH:MM". Measured on a 24-hour day.}
  \item{date}{The date to resume the program. Formatted as a character string, "MM-DD-YY". Does not function without the "time" argument.}
}
\details{
This can either calculate the delay time from the hours/minutes/seconds \bold{OR} from the date/time.  \bold{Providing arguments from both specifications will result in an error.}
\cr
\cr
Specified times must not yet have occurred.  This program utilizes a "Sys.sleep" function that is not perfectly accurate. DelayProgram will output the true delay time at the completion of the delay.
}

\author{
T.Meath
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\href{http://wp.ryot.org/wp-content/uploads/2013/01/fur_seal_pup.jpg}{seal pup}}
}
\examples{

DelayProgram(seconds=15, minutes=0, hours=0)

DelayProgram(date="12-24-2017", time="7:11")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~delay }
\keyword{ ~pause }% __ONLY ONE__ keyword per line
