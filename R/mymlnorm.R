#' Maximum Likelihood Estimation for Normal Distribution Parameters
#'
#' This function estimates the mean and standard deviation of a normal distribution using maximum likelihood estimation based on a given sample. It also provides a contour plot of the likelihood function.
#'
#' @param x   Numeric vector. The sample data for which the parameters are estimated.
#' @param mu  Numeric vector. A range of candidate values for the mean.
#' @param sig Numeric vector. A range of candidate values for the standard deviation.
#' @param ... Additional graphical parameters to be passed to the contour function.
#'
#' @returns A list containing:
#' \item{x}{The input sample data.}
#' \item{muest}{The estimated mean based on MLE.}
#' \item{sigest}{The estimated standard deviation based on MLE.}
#' \item{maxl}{The maximum likelihood value.}
#'
#' @export
#' @importFrom graphics contour
#' @importFrom stats dnorm sd
#'
#' @examples
#' \dontrun{
#' x <- c(5, 7, 7, 8, 10)  # Sample data
#' mu <- seq(4, 12, length.out = 100)  # Grid of μ values
#' sig <- seq(0.1, 5, length.out = 100)  # Grid of σ values
#'
#' mymlnorm(x, mu, sig)
#' }
mymlnorm <- function(x,mu,sig,...){  #x sample vector
  nmu=length(mu) # number of values in mu
  nsig=length(sig)
  n=length(x) # sample size
  zz=c()    ## initialize a new vector
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))   # log lik for normal
  for(j in 1:nsig){
    z=outer(x,mu,lfun,p=sig[j]) # z a matrix
    # col 1 of z contains lfun evaluated at each x with first value of mu,
    # col2 each x with 2nd value of m
    # all with sig=sig[j]
    y=apply(z,2,sum)
    # y is a vector filled with log lik values,
    # each with a difft mu and all with the same sig[j]
    zz=cbind(zz,y)
    ## zz is the matrix with each column containing log L values, rows difft mu, cols difft sigmas
  }
  maxl=max(exp(zz))
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")),...)
  mlx=round(mean(x),2)  # theoretical
  mly=round(sqrt((n-1)/n)*sd(x),2)
  #axis(1,at=c(0:20,mlx),labels=sort(c(0:20,mlx)))
  #axis(2,at=c(0:20,mly),labels=TRUE)
  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

  # Now find the estimates from the co-ords
  muest=mu[coord[1]]
  sigest=sig[coord[2]]

  abline(v=muest, h=sigest)
  return(list(x=x,muest = muest, sigest = sigest,maxl=maxl))
}
