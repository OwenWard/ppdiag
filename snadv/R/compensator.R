## Calculate compensators for different models

#' Compensators for MMHP, HP and MMPP models
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
compensator.hp <- function(object, t, pzt) {
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  n <- length(t)
  delta.t <- t - c(0, t[-n])
  Lambda <- rep(0, n)
  A <- 0
  Lambda[1] <- lambda0 * (t[1]) * 2
  for (i in 2:n) {
    A <- 1 + exp(-beta * (delta.t[i - 1])) * A
    Lambda[i] <- lambda0 * (delta.t[i]) * 2 + alpha / beta * (1 - exp(-beta * delta.t[i])) * A
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
