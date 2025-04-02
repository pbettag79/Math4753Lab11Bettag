#' Logarithm of Exponential Density Function
#'
#' Computes the logarithm of the exponential density function for given input and parameter values.
#'
#' @param lfun  A functionepresenting the log-likelihood to be maximized. It should accept two arguments: data ('x') and parameter values ('param').
#' @param x     A numeric vector of observced data points.
#' @param param A numeric vector of initial parameter values to evaluate.
#' @param delta A positive numeric value specifying the refinement step size. Default is 0.0001.
#' @param ...   Additional arguments passed to 'lfun'.
#'
#' @returns A numeric value representing the refined maximum likelihood estimate for the parameter.
#' @export
#' @importFrom graphics layout
#'
#' @examples
#' \dontrun{
#' logexp <- function(x, param) log(dexp(x,rate = param))
#'
#' refined_lambda <- mymld(
#'   lfun = logexp,
#'   x = c(5, 5, 6, 7),
#'   param = seq(0, 1, length = 1000),
#'   delta = 0.0001
#' )
#' }
mymld<-function(lfun,x,param,delta=0.0001,...){    # param = parameter values, delta=accuracy, x=data
  z=outer(x,param,lfun)    # create outer product and evaluate at lfun
  y=apply(z,2,sum) # x by param, 2=columns , sum columns = sum of log lik

  i=max(which(y==max(y)))# the index for which y is biggest, if two then take the last one

  param2=seq(param[i-2],param[i+2],by=delta)# The maximum will be between these two, increments by delta
  zz=outer(x,param2,lfun) # new z, call it zz
  yy=apply(zz,2,sum)   # new y, call it yy
  ii=max(which(yy==max(yy)))# new i,  call it ii , if two, take max of them (last one)
  layout(matrix(c(1,2),nrow = 1,ncol = 2,byrow = TRUE))# divide plotting space for two graphs
  plot(param,y,col = "Blue",type = "l",lwd = 2,ylab = "Log. Lik.",xlab = expression(theta))# plot log lik Vs parameter values
  abline(v=param[i],lwd=2,col="Red") # Show vertical line at estimated value
  axis(3,param[i],round(param[i],2))
  points(param[i],y[i],pch=19,cex=1.5,col="Black")# Plot the point
  plot(param2,yy,col="Blue",type="l",lwd=2,ylab="Log. Lik.",xlab=expression(theta),las=2) # construct new plot for refined estimate
  abline(v=param2[ii],lwd=2,col="Red")  # new verical line
  val=round(param2[ii],abs(log10(delta))) ## rounds to the nth place where n is st delta=10^-n.
  axis(3,param2[ii],val)
  points(param2[ii],yy[ii],pch=19,cex=1.5,col="Black")
  v <- param2[ii]
  v
}
