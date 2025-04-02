#' Newton-Raphson Method for Root Finding
#'
#' Implements the Newton-Raphson algorithm to find the root of a given function. The method iteratively approximates the root by using the derivative of the function.
#'
#' @param x0    Initial guess for the root.
#' @param delta Convergence threshold; the iteration stops when the absolute value of the function at the current approximation is less thanhis value. Default is 0.001.
#' @param f     A function whose root is to be found.
#' @param h     Step size for numerical differentiation. Default is 0.001.
#' @param xlim  Range of x-values for plotting. Default is c(-x0, x0).
#'
#' @returns A list containing:
#' \item{x}{Vector of approximations for the root at each iteration.}
#' \item{y}{Vector of function values corresponding to each approximation.}
#' @export
#' @importFrom graphics curve segments
#'
#' @examples
#' \dontrun{
#' # Find the root of a quadratic function
#' mynewt(x0 = 10, delta = 0.000001, f = function(x) x^2 - 5*x + 6)
#' }
mynewt<-function(x0, delta=0.001, f, h=0.001, xlim = c(-x0,x0)) {
  d <- 100
  i <- 0
  x <- c()
  y <- c()
  x[1] <- x0
  y[1] <- f(x[1])
  while(d > delta & i < 1000){
    i <- i+1
    deriv <- (f(x[i] + h/2) - f(x[i] - h/2)) / h
    x[i+1] <- x[i] - f(x[i])/deriv
    y[i+1] <- f(x[i+1])
    d <- abs(y[i])
  }
  #windows()
  curve(f(x),
        xlim = xlim,
        xaxt = "n",
        main = "Newton-Raphson Algorithm")

  points(x,
         y,
         col = "Red",
         pch = 19,
         cex = 1.5)

  axis(1,x,round(x,2),las=2)
  abline(h = 0,col = "Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Pink")

  list(x = x,y = y)
}
