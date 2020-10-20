#' Simulate homogeneous poisson process events
#'
#' @param hpp hpp object in list type, (lambda, start=0, end=1, n = NULL)
#' @param start start time of events simulated
#' @param end end time of events simulated
#' @importFrom stats runif
#' @importFrom stats rpois
#' 
#' @return a vector of length n
#' @export
#' @examples
#' hpp_obj=hpp(lambda = 1, n=50)
#' s=simulatehpp(hpp_obj, end=10)
#' hist(s)

simulatehpp <- function(hpp, start=0, end){
  lambda <- hpp$lambda
  n <- hpp$n
  if(start == end) {
    stop("Start and end time identical")
  }
  if(!is.null(n)){
    if(n==0){
      return (NULL)
    }
    if(end>start){
      message(paste(n, " events simulated, end time ignored. To simulate up to an endtime don't specify n.",
                    sep=""))
    }
    hpp <- cumsum(c(start,-log(runif(n))/lambda))
    # hpp <- round(hpp,2)
    return (hpp[2:length(hpp)])
  }else{
    n <- rpois(n=1,lambda=lambda*end)
    if(n==0){
      message("No events simulated since n is 0. ")
      return (NULL)
    }
    hpp <- (end-start)*runif(n)+start # to make this n events
    # hpp <- round(hpp,2)
    return (sort(hpp))
  }
}
