#' Simulate homogeneous poisson process events
#'
#' @param lambda rate of the poisson process
#' @param n number of events
#' @param t0 start time of events
#' @return a vector of length n
#' @export

hpp <- function(lambda,n,t0=0){
	return(t0+cumsum(rexp(n=n,rate=lambda)))
}
