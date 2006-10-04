\name{IPSURgetAnswer}
\alias{IPSURgetAnswer}
\title{Display answers to selected problems }
\description{
  Displays answers to selected problems in the system's web browser. Adapted from a function in the UsingR package by John Verzani.
}
\usage{
IPSURgetAnswer(chapter = NULL, problem = NULL)
}
\arguments{
  \item{chapter}{The chapter number}
  \item{problem}{The problems number}
  Not all answers are available.
}
\details{
  Some selected answers from the problems in \emph{Introduction to Probability & Statistics Using R} are available from the webpage
  \url{http://www.cc.ysu.edu/~gjkerns/IPSUR}. This function will display them one-by-one in the browser.
}
\value{
  If available, opens web browser to the requested answer.
}
\author{G. Jay Kerns}
\seealso{ See Also\code{\link{IPSURweb}}}
\examples{
IPSURgetAnswer()
}
\keyword{ utilities }% at least one, from doc/KEYWORDS
