#' Title mymaxlik
#'
#'
#' @importFrom graphics axis
#'
#' @param lfun The likelihood function to be maximized.
#' @param x A vector of values for the independent variable.
#' @param param A vector of parameter values to be optimized.
#' @param ... extra arguments for the plotting function
#'
#' @return A list containing the index of the maximum likelihood estimate (MLE) (i),
#' the parameter values (parami) conesponding to the indexs, the log-likelihood at this MLE (yi),
#' and the slopes of the likelihood function around the MLE (slope).
#' @export
#'
#' @examples
#' logbin=function(x,param) log(dbinom(x,prob=param,size=10))
#' mymaxlik(x=c(9,9,1,9,9,9),param=seq(0,1,length=1000),lfun=logbin,xlab=expression(pi))
mymaxlik=function(lfun,x,param,...){
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z=outer(x,param,lfun)
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
