\name{IPSURweb}
\alias{IPSURweb}
\alias{ipsurweb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Opens browser to IPSUR webpages }
\description{
  Opens the browser to webpages from the IPSUR website. Adapted from a function in the UsingR package by John Verzani.
}
\usage{
IPSURweb(what = c("homepage", "errata", "changes", "exercises", "package"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{A character string indicating what page to open. No value
  specified will open the IPSUR homepage. Others are
  "errata" for typos and errors, "changes" for changes to the text
  brought about by new changes to R, "exercises" to go to a page
  containing selected answers, and "packages" for materials related to the IPSUR package.}
}
\value{
  Opens the browser to the respective web page.
}
\references{ see \url{http://www.cc.ysu.edu/~gjkerns} }
\author{ G. Jay Kerns }

\seealso{ See Also\code{\link{IPSURgetAnswer}}}
\examples{
\dontrun{IPSURweb()}			# main webpage
\dontrun{IPSURweb("err")}			# errata
}
\keyword{ misc }% at least one, from doc/KEYWORDS
