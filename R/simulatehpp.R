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
#' @noRd
#' @examples
#' hpp_obj=hpp(lambda = 1)
#' s=pp_simulate(hpp_obj, end = 10, n=50)
#' hist(s)

simulatehpp <- function(hpp, start=0, end=NULL, n=NULL, seed=NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  old_events <- hpp$events
  if(!is.null(old_events)){
    message("Events in the hpp object will be overwritten by simulated events.")
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
    events <- cumsum(c(0,-log(runif(n))/lambda))
    hpp$events <- events[2:length(events)]
    return (hpp$events)
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
      events <- c()
      while(t<=end){
        events <- append(events,t)
        u <- runif(1)
        while(u==0){
          u <- runif(1)
        }
        t <- t-log(u)/lambda
      }
      hpp$events <- sort(events)
      return (hpp$events)
    }

  }
}
