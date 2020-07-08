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
#' simulatehpp(hpp_obj)

simulatehpp <- function(hpp){
  lambda=hpp$lambda
  end=hpp$end
  start=hpp$start
  n=hpp$n
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
