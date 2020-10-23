#' Simulate homogeneous poisson process events
#'
#' @param hpp hpp object in list type
#' @param start start time of events simulated
#' @param end end time of events simulated
#' @param n number of events
#' @param seed seed for simulation
#' @importFrom stats runif
#' @importFrom stats rpois
#' @return a vector of length n
#' @export
#' @examples
#' hpp_obj=hpp(lambda = 1)
#' s=simulatehpp(hpp_obj, end = 10, n=50)
#' hist(s)

simulatehpp <- function(hpp, start=0, end=NULL, n=NULL, seed=NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  old_events <- hpp$events
  if(!is.null(old_events)){
    stop("Event time already in the hpp object.")
  }
  
  if(!is.null(start) && !is.null(end)){
    if(start >= end){
      return (NULL)
    }
  }
  
  lambda <- hpp$lambda
  
  if(!is.null(n)){
    if(n<=0){
      stop("n must be positive for simulation.")
    }
    if(!is.null(end)){
      message(paste(n, " events simulated. To simulate up to an endtime set n=NULL.",
                    sep=""))
    }
    hpp <- cumsum(c(0,-log(runif(n))/lambda))
    return (hpp[2:length(hpp)])
  }else{
    if(is.null(end)){
      stop("Specify either endtime or n to simulate events. ")
    }else{
      message("Simulating up to endtime. To simulate n events specify n.")
      
      u <- runif(1)
      while(u==0){
        u <- runif(1)
      }
      t <- -log(u)/lambda
      hpp <- c()
      while(t<=end){
        hpp <- append(hpp,t)
        u <- runif(1)
        while(u==0){
          u <- runif(1)
        }
        t <- t-log(u)/lambda
      }
      
      return (sort(hpp))
    }

  }
}
