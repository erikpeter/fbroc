% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fbroc.methods.R
\name{print.fbroc.roc}
\alias{print.fbroc.roc}
\title{Prints information about a \code{fbroc.roc} object}
\usage{
\method{print}{fbroc.roc}(x, ...)
}
\arguments{
\item{x}{Object of class \code{fbroc.roc}.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
Character containing the text that is also printed.
}
\description{
Prints the information about the bootstrap results for an object of class
\code{fbroc.roc}. This information includes the number of bootstrap
replicates, the time spent on bootstrapping, the AUC and the memory
usage of the object.
}
\seealso{
\code{\link{boot.roc}}
}
