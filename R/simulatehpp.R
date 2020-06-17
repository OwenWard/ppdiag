#' Simulate homogeneous poisson process events
#'
#' @param lambda rate of the poisson process
#' @param n number of events
#' @param start start time of events
#' @param end end time of events
#' 
#' @return a vector of length n
#' @export

simulatehpp <- function(lambda,start,end=0,n=NULL){
  if(!is.null(n)){
    if(end>start){
      cat("hpp events with length n will be generated, end time might be 
          different from the value you entered, if you have a preferred 
          end time, please rerun the function without specifying n.")
    }
    hpp=cumsum(c(start,-log(runif(n-1))/lambda))
    return (hpp[2:length(hpp)])
  }else{
    n=rpois(n=1,lambda=lambda*end)
    if(n==0){
      return (NULL)
    }
    hpp=(end-start)*runif(n-1)+start
    return (sort(hpp))
  }
}
