#' Return the intensity function of Hawkes Process
#'
#' Return the intensity function of Hawkes Process, a helper function for 'UniMMHPIntensity'
#'
#' @param lambda parameters for Hawkes process
#' @param i state number
#' @param beta parameters for Hawkes process
#' @param alpha parameters for Hawkes process
#' @param start the start time of current state
#' @param end the end time of current state
#' @param history the past event times
#' @param hawkes_time the event times happened in this state

#' @export


HPIntensity <- function (lambda, i, alpha, beta, start, end, history, hawkes_time) {
  n <- length(hawkes_time)
  m <- length(history)
  intensity <- function(x){
    y <- 0
    if(n == 0){
      if(i == 1){
        if(x >= start & x <= end){
          y <- lambda
        }
      } else{
        lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        if(x >= start & x <= end){
          y <- new.lambda.n(x)
        }
      }
    }else{
      if(i == 1){
        if(x >= start & x < hawkes_time[1]){
          y <- lambda
        }
      } else{
        lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s ,m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        if(x >= start & x < hawkes_time[1]){
          y <- new.lambda.n(x)
        }
      }
      if(n > 1){
        for(j in 1:(n-1)){
          lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m + j) - c(history, hawkes_time[1:j]))))
          new.lambda.n <- Vectorize(lambda.n)
          if(x >= hawkes_time[j] & x < hawkes_time[j + 1]){
            y <- new.lambda.n(x)
          }
        }
      }
      lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m + n) - c(history, hawkes_time[1:n]))))
      new.lambda.n <- Vectorize(lambda.n)
      if(x >= hawkes_time[n] & x <= end){
        y <- new.lambda.n(x)
      }
    }
    return(y)
  }
  return(Vectorize(intensity))
}
