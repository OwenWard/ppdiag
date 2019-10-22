#' Compute negative log likelihood for Markov-modulated Hawkes Process(MMHP) model
#'
#' Compute negative log likelihood for MMHP model with model specified time events or simulated time events
#'
#' @param mmhp a mmhp object including its Q, delta, tau, lambda0, lambda1, beta and alpha.
#' @param t vector containing the event times. Note that if `mmhp` obejct does have argument `tau`, then `t` is not needed anymore.
#'
#' @return a scalar indicating the negative log likelihood
#' @export
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1, alpha = 0.8, beta = 1.2)
#' y <- simulatemmhp(x, nsim = 5)
#' mmhpNegLogLik(x, y$tau)
mmhpNegLogLik <- function(mmhp, t = NULL) {
  # t is event time, t[1]=0
  lambda0 <- mmhp$lambda0
  lambda1 <- mmhp$lambda1
  alpha <- mmhp$alpha
  beta <- mmhp$beta
  q1 <- mmhp$Q[1, 2]
  q2 <- mmhp$Q[2, 1]
  if (!is.null(mmhp$tau)) {
    t <- mmhp$tau
  }

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
