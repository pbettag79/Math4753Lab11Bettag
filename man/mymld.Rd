% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mymld.R
\name{mymld}
\alias{mymld}
\title{Logarithm of Exponential Density Function}
\usage{
mymld(lfun, x, param, delta = 1e-04, ...)
}
\arguments{
\item{lfun}{A functionepresenting the log-likelihood to be maximized. It should accept two arguments: data ('x') and parameter values ('param').}

\item{x}{A numeric vector of observced data points.}

\item{param}{A numeric vector of initial parameter values to evaluate.}

\item{delta}{A positive numeric value specifying the refinement step size. Default is 0.0001.}

\item{...}{Additional arguments passed to 'lfun'.}
}
\value{
A numeric value representing the refined maximum likelihood estimate for the parameter.
}
\description{
Computes the logarithm of the exponential density function for given input and parameter values.
}
\examples{
\dontrun{
logexp <- function(x, param) log(dexp(x,rate = param))

refined_lambda <- mymld(
  lfun = logexp,
  x = c(5, 5, 6, 7),
  param = seq(0, 1, length = 1000),
  delta = 0.0001
)
}
}
