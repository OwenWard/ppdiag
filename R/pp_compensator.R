#' Compensators for point processes
#' 
#' Computes the compensator for included point processes
#'
#' @param object a social network model
#' @param events event times, which can have first value as 0
#' @return compensator vector of rescaled interevent times
#' @export
#' @examples 
#' hpp_obj <- pp_hpp(lambda = 1)
#' events <- pp_simulate(hpp_obj, end=10)
#' comp <- pp_compensator(hpp_obj, events)

pp_compensator <- function(object, events) {
  UseMethod("pp_compensator")
}

#' @rdname pp_compensator
#' @export
pp_compensator.default <- function(object, events) {
  cat("Please input the right model. Select from hp, hpp and mmhp.")
}

#' @rdname pp_compensator
#' @export
pp_compensator.mmpp <- function(object, events) {
  lambda0 <- object$lambda0
  c <- object$c
  Q <- object$Q
  q1 <- Q[1, 2]
  q2 <- Q[2, 1]
  if(events[1] == 0) {
    n <- length(events) - 1
    interevent <- events[-1] - events[-(n + 1)]
    pzt <- mmpp_event_state(params = object, events[-1])$pzt
  }
  else {
    n <- length(events)
    interevent <- diff(c(0,events))
    pzt <- mmpp_event_state(params = object, events)$pzt
  }
  interevent <- events[-1] - events[-(n + 1)]
  Lambda_mixed <- lambda0 * (1 + c) * interevent * pzt + 
    lambda0 * interevent * (1 - pzt)
  return(Lambda_mixed)
}

#' @rdname pp_compensator
#' @export
pp_compensator.hp <- function(object, events) {
  # input object: parameters for Hawkes process, include lambda0, alpha, beta 
  #       events: vector of events times
  # output Lambda: vector of compensator evaluated at each event time
  
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  if(events[1] == 0 ){
    events <- events[-1]
  }
  if(length(events) == 0) {
    return(0)
  }
  N <- length(events)
  Lambda <- rep(0,N)
  r <- 0
  Lambda[1] <- lambda0*(events[1]) # start at 0
  if(N == 1) {
    return(Lambda)
  }
  for(i in 2:N){
    delta.t <- events[i]-events[i-1]
    temp.r <- exp(-beta*delta.t)*(r+1)
    Lambda[i] <- lambda0*delta.t-alpha/beta*(temp.r-r-1)
    r <- temp.r
  }
  return(Lambda)

}

#' @rdname pp_compensator
#' @export
pp_compensator.mmhp <- function(object, events) {
  lambda0 <- object$lambda0
  lambda1 <- object$lambda1
  alpha <- object$alpha
  beta <- object$beta
  q1 <- object$Q[1, 2]
  q2 <- object$Q[2, 1]
  if(length(events) == 0){
    return(0)
  }
  if(events[1] == 0) {
    n <- length(events) - 1
    interevent <- events[-1] - events[-(n + 1)]
    pzt <- mmhp_event_state(params = object, events[-1])$pzt
  }
  else {
    n <- length(events)
    interevent <- diff(c(0,events))
    pzt <- mmhp_event_state(params = object, events)$pzt
  }
  ## compute compensator for Hawkes process
  Lambda <- rep(0, n)
  A <- 0
  Lambda[1] <- lambda0 * (interevent[1])
  if( n > 1) {
    for (i in 2:n) {
      A <- 1 + exp(-beta * (interevent[i - 1])) * A
      Lambda[i] <- lambda1 * (interevent[i]) + 
        alpha / beta * (1 - exp(-beta * interevent[i])) * A
    }
  }
  Lambda_mixed <- Lambda * pzt + lambda0 * interevent * (1 - pzt)
  return(Lambda_mixed)
}

#' @rdname pp_compensator
#' @export
pp_compensator.hpp <- function(object, events) {
  if(events[1] == 0) {
    events <- events[-1]
  }
  if(length(events) == 0) {
    return(0)
  }
  # if(length(events) == 1) {
  #   return( (events - 0)*lambda )
  # }
  # if(length(events == 2)) {
  #   Lambda <-
  # }
  # N <- length(events)
  # lambda <- object$lambda
  # Lambda <- rep(0,N)
  # for (i in 2:N){
  #   Lambda[i] <- (events[i]-events[i-1])*lambda
  # }
  lambda <- object$lambda
  Lambda <- diff(c(0,events) ) *lambda
  return(Lambda)
}
