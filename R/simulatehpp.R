#' Simulate homogeneous poisson process events
#'
#' @param lambda rate of the poisson process
#' @param n number of events
#' @param start start time of events
#' @param end end time of events
#' @importFrom stats runif
#' @importFrom stats rpois
#' 
#' @return a vector of length n
#' @export

simulatehpp <- function(lambda,end = 1,start = 0,n=NULL){
  if(start == end) {
    stop("Start and end time identical")
  }
  if(!is.null(n)){
    if(end>start){
      message("n events simulated, end time specified will be ignored. To simulate events up to an endtime do not specify n.")
    }
    hpp=cumsum(c(start,-log(runif(n))/lambda))
    return (hpp[2:length(hpp)])
  }else{
    n=rpois(n=1,lambda=lambda*end)
    if(n==0){
      return (NULL)
    }
    hpp=(end-start)*runif(n)+start # to make this n events
    return (sort(hpp))
  }
}
