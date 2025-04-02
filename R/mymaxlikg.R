#' Log-Likelihood Function for Three Binomial Experiments
#'
#' Computes the log-likelihood for three binomial experiments given a probability parameter theta.
#'
#' @param lfun  A character string specifying the name of the log-likelihood function to be used.  The default is "logbin3".
#' @param theta A numeric vector of probability values to evaluate the likelihood function.
#'
#' @returns The value of theta corresponding to the maximum likelihood.
#' @export
#' @importFrom graphics title
#'
#' @examples
#' \dontrun{
#' logbin3 <- function(theta){
#' log(dbinom(4, prob = theta, size = 10)) +  # X1 = 4/10
#' log(dbinom(12, prob = theta, size = 20)) + # X2 = 12/20
#' log(dbinom(7, prob = theta, size = 15))    # X3 = 7/15
#' }
#'
#' mymaxlikg(theta=seq(0,1,length=10000))
#' }
mymaxlikg <- function(lfun="logbin3", theta) { # default log lik is a combination bin
  nth <- length(theta)  # nu. of values used in theta
  thmat <- matrix(theta, nrow=nth, ncol=1, byrow=TRUE) # Matrix of theta
  z <- apply(thmat, 1, lfun) # z holds the log lik values
  zmax <- max(which(z==max(z)))  # finding the INDEX of the max lik
  plot(theta, exp(z), type="l",
       xlab = "Probability (p)",
       ylab = "Likelihood",
       main = "") # plot of lik
  title(main = "MLE for Three Binomial Experiments", adj = 0)
  abline(v = theta[zmax], col="blue")   #  vertical line through max
  axis(3, theta[zmax], round(theta[zmax], 4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}
