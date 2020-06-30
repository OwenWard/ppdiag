## Calculate compensators for different models

#' Compensators for MMHP, HP HPP, and MMPP models
#'
#' @param object a social network model
#' @param pzt probability
#' @param t event times
#' @return vector of compensator
#' @export
#'

compensator <- function(object, t, pzt) {
  UseMethod("compensator")
}

#' @rdname compensator
#' @export
compensator.default <- function(object, t, pzt) {
  cat("please input the right model")
}

#' @rdname compensator
#' @export
compensator.mmpp <- function(object, t, pzt) {
  lambda0 <- object$lambda0
  c <- object$c
  q1 <- object$q1
  q2 <- object$q2
  n <- length(t) - 1
  interevent <- t[-1] - t[-(n + 1)]
  Lambda_mixed <- lambda0 * (1 + c) * interevent * pzt + lambda0 * interevent * (1 - pzt)
  return(Lambda_mixed)
}

#' @rdname compensator
#' @export
compensator.hp <- function(object, t, pzt = NULL) {
  # input object: parameters for Hawkes process, include lambda0, alpha, beta 
  #       events: vector of events times
  # output Lambda: vector of compensator evaluated at each event time
  
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  
  N<-length(t)
  Lambda<-rep(0,N)
  r<-0
  Lambda[1]<-lambda0*(t[1])
  for(i in 2:N){
    delta.t <- t[i]-t[i-1]
    temp.r <-exp(-beta*delta.t)*(r+1)
    Lambda[i]<-lambda0*delta.t-alpha/beta*(temp.r-r-1)
    r <- temp.r
  }
  return(Lambda)

}

#' @rdname compensator
#' @export
compensator.mmhp <- function(object, t, pzt) {
  lambda0 <- object$lambda0
  lambda1 <- object$lambda1
  alpha <- object$alpha
  beta <- object$beta
  q1 <- object$Q[1, 2]
  q2 <- object$Q[2, 1]
  n <- length(t) - 1
  interevent <- t[-1] - t[-(n + 1)]
  # if(if.pzt){
  #  pzt <- mmhpLocalState(params=params, interevent)$pzt
  # }

  ## compute compensator for Hawkes process
  Lambda <- rep(0, n)
  A <- 0
  Lambda[1] <- lambda0 * (interevent[1])
  for (i in 2:n) {
    A <- 1 + exp(-beta * (interevent[i - 1])) * A
    Lambda[i] <- lambda1 * (interevent[i]) + alpha / beta * (1 - exp(-beta * interevent[i])) * A
  }
  Lambda_mixed <- Lambda * pzt + lambda0 * interevent * (1 - pzt)
  return(Lambda_mixed)
}

#' @rdname compensator
#' @export
compensator.hpp <- function(object, t, pzt = NULL) {
  N=length(t)
  lambda=object$lambda
  Lambda=rep(0,N)
  for (i in 2:N){
    Lambda[i]=(t[i]-t[i-1])*lambda
  }
  return(Lambda)
}
