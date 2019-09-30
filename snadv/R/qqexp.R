#' Plot Quantile-quantile plot for rescaled-inter-event-times
#'
#' Generate Quantile-quantile plot for rescaled-inter-event-times, which are supposedly to be independently and identically distributed as exponential random vairbales with rate 1.
#' 
#' @param r rescaled-inter-event-times
#' @param ... other arguments for plots
#' @importFrom stats ppoints
#' @importFrom stats quantile
#' @importFrom stats qexp
#' @importFrom stats qqline

#' @export



qqexp <-function(r,...){
  p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
q <- quantile(r,p=p) # percentiles of the sample distribution
plot(qexp(p) ,q, main="Exponential Q-Q Plot",
     xlab="Theoretical Quantiles",ylab="Sample Quantiles",...)
qqline(q, distribution=qexp,col="blue", lty=2)
}

