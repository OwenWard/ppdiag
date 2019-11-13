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
#' @param method the method used to calculate the MMHP intensity. The candidates are: `function` and `numeric`, default to `function`.
#' @param time.vec time segment to calculate the intensity for `numeric` method
#' @param t the observed true event time

#' @export


HPIntensity <- function(lambda, alpha, beta, i=NULL, start=NULL, end=NULL, history=NULL, hawkes_time=NULL,method =  "function",time.vec=NULL,t=NULL) {
  if(method =="function"){
    n <- length(hawkes_time)
    m <- length(history)
    intensity <- function(x) {
      y <- 0
      if (n == 0) {
        if (i == 1) {
          if (x >= start & x <= end) {
            y <- lambda
          }
        } else {
          lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m) - history)))
          new.lambda.n <- Vectorize(lambda.n)
          if (x >= start & x <= end) {
            y <- new.lambda.n(x)
          }
        }
      } else {
        if (i == 1) {
          if (x >= start & x < hawkes_time[1]) {
            y <- lambda
          }
        } else {
          lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m) - history)))
          new.lambda.n <- Vectorize(lambda.n)
          if (x >= start & x < hawkes_time[1]) {
            y <- new.lambda.n(x)
          }
        }
        if (n > 1) {
          for (j in 1:(n - 1)) {
            lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m + j) - c(history, hawkes_time[1:j]))))
            new.lambda.n <- Vectorize(lambda.n)
            if (x >= hawkes_time[j] & x < hawkes_time[j + 1]) {
              y <- new.lambda.n(x)
            }
          }
        }
        lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m + n) - c(history, hawkes_time[1:n]))))
        new.lambda.n <- Vectorize(lambda.n)
        if (x >= hawkes_time[n] & x <= end) {
          y <- new.lambda.n(x)
        }
      }
      return(y)
    }
    return(Vectorize(intensity))
  }else if(method =="numeric"){

    lambda1.t <- rep(0,length(time.vec))
    event.idx <- 1

    r <- 0
    for(i in c(1:length(time.vec))){
      current.t <- time.vec[i]
      if(event.idx < length(t)){
        if(current.t>t[event.idx+1]){
          event.idx <- event.idx + 1
          r <- exp(-beta*(t[event.idx]-t[event.idx-1]))*(1+r)
        }
      }

      if(current.t<=t[1]){
        lambda1.t[i]<-lambda
      }else{
        lambda1.t[i]<-lambda+alpha*exp(-beta*(current.t-t[event.idx]))*(1+r)
      }
    }

    return(lambda1.t)

  }
}
