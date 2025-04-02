#' Find Maximum Likelihood Estimate
#'
#' This function calculates the maximum likelihood estimate (MLE) for a given log-likelihood function, a set of data points, and a range of parameter values. It visualizes the likelihood function and identifies the parameter value that maximizes it.
#'
#' @param lfun  A function representing the log-likelihood. It should accept two arguments: data points('x') and parameter values ('param').
#' @param x     A numeric vector of data points.
#' @param param A numeric vector of parameter values to evaluate the log-likelihood function.
#' @param ...   Additional graphical parameters for the plot.
#'
#' @returns A list containing:
#' \item{i}{Index of the parameter value that maximizes the likelihood.}
#' \item{parami}{The parameter value corresponding to the maximum likelihood.}
#' \item{yi}{The maximum log-likelihood value.}
#' \item{slope}{The slopes around the maximum likelihood point, if applicable.}
#' @export
#' @importFrom graphics abline axis points
#'
#' @examples
#' \dontrun{
#' # Example usage with a binomial log-likelihood function
#' logbin <- function(x, param) dbinom(x, size = 10, prob = param, log = TRUE)
#' v <- mymaxlik(
#'   x = c(9, 10, 11),
#'   param = seq(0, 1, length = 1000),
#'   lfun = logbin,
#'   xlab = expression(p),
#'   main = "Binomial MLE for p",
#'   cex.main = 2
#' )
#' }
mymaxlik <- function(lfun,x,param,...){
  # how manyparam values are there?
  np <- length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z <- outer(x,param,lfun)
  # z is a matrix where each x,param is replaced with the function evaluated at those values

  y <- apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value

  plot(param,
       y,
       col="Blue",
       type="l",
       lwd=2,
       ...)

  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger index
  i <- max(which.max(y))

  abline(v=param[i],
         lwd=2,
         col="Red")

  # plots a nice point where the max lik is
  points(param[i],
         y[i],
         pch=19,
         cex=1.5,
         col="Black")

  axis(3,
       param[i],
       round(param[i],2))

  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3 >= 1 & i+2 <= np,
         slope <-  (y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),
         slope <- "NA")

  return(invisible(list(i = i,
                        parami = param[i],
                        yi = y[i],
                        slope = slope)
  ))
}
