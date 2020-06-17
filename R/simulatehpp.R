#' Simulate homogeneous poisson process events
#'
#' @param lambda rate of the poisson process
#' @param n number of events
#' @param start start time of events
#' @param end end time of events
#' 
#' @return a vector of length n
#' @export

simulatehpp <- function(lambda,start=0,end=0,n=NULL){
  if(!is.null(n)){
    if(end>start){
      cat("hpp events with length n will be generated, end time might be 
          different from the value you entered, if you have a preferred 
          end time, please rerun the function without specifying n.")
    }
    return(cumsum(c(start,-log(runif(n-1))/lambda)))
  }else{
    n=rpois(n=1,lambda=lambda*end)
    u=(end-start)*runif(n-1)+start
    return (sort(c(start,u)))
  }
}
