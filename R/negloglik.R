#' Compute negative log likelihood for social network models
#'
#' Compute negative log likelihood for social network models with model specified time events or simulated time events
#'
#' @param object social network model contating the parameters
#' @param t vector containing the event times.
#' @param end the end time of event times
#'
#' @return a scalar indicating the negative log likelihood
#' @export

negloglik <- function(object, t, end) {
  UseMethod("negloglik")
}

#' @rdname negloglik
#' @export
negloglik.default <- function(object, t, end) {
  cat("please input the right model")
}

#' @rdname negloglik
#' @export
negloglik.hp <- function(object, t, end) {
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  n <- length(t)
  r <- rep(0, n)
  if (n > 2) {
    for (i in 2:n) {
      r[i] <- exp(-beta * (t[i] - t[i - 1])) * (1 + r[i - 1])
    }
  }
  if (is.null(end)) {
    loglik <- -t[n] * lambda0
    end <- t[n]
  } else {
    loglik <- -end * lambda0
  }
  loglik <- loglik + alpha / beta * sum(exp(-beta * (end - t)) - 1)
  if (any(lambda0 + alpha * r <= 0)) {
    loglik <- -1e+10
  } else {
    loglik <- loglik + sum(log(lambda0 + alpha * r))
  }
  return(-loglik)
}

#' @rdname negloglik
#' @export

negloglik.mmhp <- function(object, t, end) {
  # t is event time, t[1]=0
  lambda0 <- object$lambda0
  lambda1 <- object$lambda1
  alpha <- object$alpha
  beta <- object$beta
  q1 <- object$Q[1, 2]
  q2 <- object$Q[2, 1]

  n <- length(t) - 1
  interevent <- t[-1] - t[-(n + 1)]

  forward <- matrix(0, ncol = 2, nrow = n)
  probs_1 <- matrix(0, ncol = 2, nrow = n) # Probability vector for transition to state 1 (active state)
  probs_2 <- matrix(0, ncol = 2, nrow = n) # Probability vector for transition to state 2 (inactive state)
  r <- rep(0, n)

  integ1 <- interevent[1] * lambda1
  integ2 <- interevent[1] * lambda0

  probs_1[, 1] <- -q1 * interevent
  probs_2[, 2] <- -q2 * interevent
  probs_1[, 2] <- log(1 - exp(probs_2[, 2]))
  probs_2[, 1] <- log(1 - exp(probs_1[, 1]))

  forward[1, 1] <- log(lambda1) - integ1
  forward[1, 2] <- log(lambda0) - integ2

  for (i in 2:n) {
    integ1 <- interevent[i] * lambda1
    integ2 <- interevent[i] * lambda0
    r[i] <- exp(-beta * interevent[i]) * (r[i - 1] + 1)
    a <- min(forward[i - 1, ] + probs_1[i - 1, ])
    forward[i, 1] <- a + log(sum(exp(forward[i - 1, ] + probs_1[i - 1, ] - a))) + log(lambda1 + alpha * exp(-beta * interevent[i]) * (r[i - 1] + 1)) - integ1 +
      alpha / beta * (r[i] - r[i - 1] - 1)
    a <- min(forward[i - 1, ] + probs_2[i - 1, ])
    forward[i, 2] <- a + log(sum(exp(forward[i - 1, ] + probs_2[i - 1, ] - a))) + log(lambda0) - integ2
  }
  return(-sum(forward[n, ]))
}
