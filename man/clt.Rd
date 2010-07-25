\name{The Central Limit Theorem}
\alias{The Central Limit Theorem}
\alias{clt1}
\alias{clt2}
\alias{clt3}

\title{Investigating the Central Limit Theorem}
\description{
  These functions were written for students to investigate the Central Limit Theorem.  For more information, see the exercises at the end of the chapter "Sampling Distributions" in IPSUR.
}

\usage{
clt1(population = "rt", r = 3, sample.size = 2, N.iter = 100000)
clt2(population = "runif", a = 0, b = 10, sample.size = 2, N.iter = 100000)
clt3(population = "rgamma", alpha = 1.21, theta = 2.37, sample.size = 2, N.iter = 100000)
}

\arguments{
  \item{population}{the name of a population distribution, in its random generator form.}
  \item{sample.size}{the sample size.}
  \item{N.iter}{the number of samples desired.}
  \item{r}{the degrees of freedom for Student's t distribution.}
  \item{a}{the minimum value of a continuous uniform distribution.}
  \item{b}{the maximum value of a continuous uniform distribution.}
  \item{alpha}{the shape parameter of a gamma distribution.}
  \item{theta}{the scale parameter of a gamma distribution.}
}

\details{
 When the functions are called a plot window opens to show a graph of the PDF of the population distribution. On the display are shown numerical values of the population mean and variance. The computer simulates random samples of size \code{sample.size} from the distribution a total of 
\code{N.iter} times, and sample means are calculated for each sample. Next follows a histogram of the simulated sample means, which closely approximates the sampling distribution of the sample mean.
Also shown are the sample mean and sample variance of all of the simulated sample means. As a final step, when the user clicks the second plot, a normal curve with the same mean and variance as the simulated sample means is superimposed over the histogram.
}


\author{G. Jay Kerns \email{gkerns@ysu.edu}}

\keyword{misc}
