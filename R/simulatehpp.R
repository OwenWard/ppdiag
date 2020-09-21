#' Simulate homogeneous poisson process events
#'
#' @param hpp hpp object in list type, (lambda, start=0, end=1, n = NULL)
#' @importFrom stats runif
#' @importFrom stats rpois
#' 
#' @return a vector of length n
#' @export
#' @examples
#' hpp_obj=hpp(lambda = 1, end = 10, n=50)
#' s=simulatehpp(hpp_obj)
#' hist(s)

simulatehpp <- function(hpp){
  lambda=hpp$lambda
  end=hpp$end
  start=hpp$start
  n=hpp$n
  if(start == end) {
    stop("Start and end time identical")
  }
  if(!is.null(n)){
    if(n==0){
      return (NULL)
    }
    if(end>start){
      message(paste(n, " events simulated, end time specified will be ignored. To simulate events up to an endtime do not specify n.",
                    sep=""))
    }
    hpp=cumsum(c(start,-log(runif(n))/lambda))
    hpp=round(hpp,2)
    return (hpp[2:length(hpp)])
  }else{
    n=rpois(n=1,lambda=lambda*end)
    if(n==0){
      message("No events simulated, please resimulate. The argument n is the number of events needed. ")
      return (NULL)
    }
    hpp=(end-start)*runif(n)+start # to make this n events
    hpp=round(hpp,2)
    return (sort(hpp))
  }
}
