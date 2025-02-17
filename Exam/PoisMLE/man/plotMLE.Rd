% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMLE.R
\name{plotMLE}
\alias{plotMLE}
\title{Plotting function for PoisMLE}
\usage{
plotMLE(y, SEtype, B = 1000)
}
\arguments{
\item{y}{The vector of observed data}

\item{SEtype}{One of two methods of calculation; basic or bootstrap}

\item{B}{The number of bootstrapped resamplings if using SEtype="bootstrap". Default set to 1000.}
}
\value{
plot
\item{plot}{A plot for PoisMLE that shows the MLE plus and minus 1.96 standard errors.}
}
\description{
plots the MLE with a confidence interval of plus and minus 1.96 standard errors. Used for object of class \code{PoisMLE}.
}
\note{
This is used internally for the \code{estimatePois} function
}
\examples{

set.seed(1227)
y <- rpois(1227,130)
plotMLE(y, "basic")
plotMLE(y, "bootstrap", 1000)
}
\seealso{
\code{\link{estimatePois}}

\code{\link{logLik}}

\code{\link{mle}}

\code{\link{standardError}}
}
\author{
Annie Jarman
}
