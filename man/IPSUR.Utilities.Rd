\name{IPSUR.Utilities}
\alias{IPSUR.Utilities}
\alias{checkMultiLevelFactors}
\alias{listMultiLevelFactors}
\alias{MultiLevelFactors}
\alias{multiLevelFactorsP}


\title{IPSUR Utility Functions}
\description{
  These functions support writing additions to the IPSUR package, and were
  patterned after their Rcmdr equivalents. Additional R code
  can be placed in files with file type \code{.R} in the \code{etc} subdirectory
  of the package. Add menus, submenus, and menu items by editing the file
  \code{Rcmdr-menus.txt} in the same directory.
}

\usage{
checkMultiLevelFactors(n=1)
listMultiLevelFactors(dataSet=ActiveDataSet())
MultiLevelFactors(names)
multiLevelFactorsP(n=1)
}

\arguments{
  \item{dataSet}{the quoted name of a data frame in memory.}
  \item{names}{optional names to be stored.}
  \item{n}{number of variables to check for.}
}

\details{

  There are several groups of functions exported by the \code{Rcmdr} package and documented briefly here.
  To see how these functions work, it is simplest to examine the dialog-generating functions in the \code{Rcmdr} package.


  \emph{Checking for errors: }
  The function \code{checkMultiLevelFactors}
  checks for the existence of objects and writes an error message to the log if it is absent
  (or insufficiently numerous, in the case of different kinds of variables).
  
  \emph{Information: }
  The following function returns vectors of object names:
  \code{listMultiLevelFactors}


}

\references{
    T. Lumley (2001) Programmer's niche: Macros in R.
    \emph{R News}, \bold{1(3)}, 11--13.
    }

\author{G. Jay Kerns \email{gkerns@ysu.edu}}

\keyword{misc}
