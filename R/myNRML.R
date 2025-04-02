#' Newton-Raphson Method for Log-Likelihood Optimization
#'
#' This function implements the Newton-Raphson method to optimize the log-likelihood function. It iteratively find better approximations to the roots of the derivative of the log-likelihood function.
#'
#' @param x0        Initial guess for the root.
#' @param delta     Step size for numerical differentiation. Default is 0.001.
#' @param llik      Log-likelihood function to be optimized.
#' @param xrange    Range of values for plotting the log-likelihood and its derivative.
#' @param parameter Name of the parameter being optimized (used for axis labels in plots). Default is "param".
#'
#' @returns A list containing:
#' \item{x}{Vector of successive approximations to the root.}
#' \item{y}{Vector of derivative values at each iteration.}
#' @export
#'
#' @examples
#' \dontrun{
#' # Example using Poisson log-likelihood:
#' myNRML(x0 = 2, delta = 0.000001,
#'        llik = function(x)
#'        log(dpois(3, lambda = x) * dpois(4, lambda = x)),
#'        xrange = c(0.01, 10),
#'        parameter = "lambda")
#' }
myNRML <- function(x0,delta = 0.001,llik,xrange,parameter = "param"){
  f <- function(x) (llik(x+delta)-llik(x))/delta
  fdash <- function(x) (f(x+delta)-f(x))/delta
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<100){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i+1])
  }
  layout(matrix(1:2,nrow=1,ncol=2,byrow=TRUE),widths=c(1,2))
  curve(llik(x), xlim=xrange,xlab=parameter,ylab="log Lik",main="Log Lik")
  curve(f(x),xlim=xrange,xaxt="n", xlab=parameter,ylab="derivative",main=  "Newton-Raphson Algorithm \n on the derivative")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")

  list(x=x,y=y)
}
