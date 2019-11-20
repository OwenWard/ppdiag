#' Return the intensity function of the Markov-modulated Hawkes Process(MMHP)
#'
#' Take a mmhp object and generate its intensity function accordingly
#'
#' @param mmhp a mmhp object including its state, state_time, tau, lambda0, lambda1, beta and alpha.
#' @param event the observed/simulated Markov-modulated Hawkes Process(MMHP)
#' @param method the method used to calculate the MMHP intensity.
#'   The candidates are: `function`, `numeric`, and `atevent`, default to `function`.
#' @return The intensity function of MMHP
#' @export
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1, alpha = 0.8, beta = 1.2)
#' y <- simulatemmhp(x)
#' z <- UniMMHPIntensity(x, y)
UniMMHPIntensity <- function(mmhp, event, method = "function") {
  t <- event$tau
  lambda0 <- mmhp$lambda0
  lambda1 <- mmhp$lambda1
  alpha <- mmhp$alpha
  beta <- mmhp$beta
  n <- length(t)
  if (method == "function") {
    state <- event$z
    state_time <- event$x
    m <- length(state)
    intensity <- function(x) {
      y <- 0
      for (i in 1:(m - 1)) {
        if (state[i] == 1) {
          hawkes_time <- t[t >= state_time[i] & t < state_time[i + 1]]
          if (i == 1) hawkes_time <- hawkes_time[-1]
          history <- t[t < state_time[i]]
          HPfunc <- HPIntensity(lambda1, i, alpha, beta, state_time[i], state_time[i + 1], history[-1], hawkes_time)
          if (x >= state_time[i] & x <= state_time[i + 1]) {
            y <- HPfunc(x)
          }
        } else {
          if (x >= state_time[i] & x <= state_time[i + 1]) {
            y <- lambda0
          }
        }
      }
      return(y)
    }
    return(Vectorize(intensity))
  } else if (method == "numeric") {
    time.vec <- event$time_segment
    latent.vec <- event$latent_mean
    lambda1.t <- HPIntensity(
      lambda = lambda1, alpha = alpha,
      beta = beta, t = t, time.vec = time.vec, method = "numeric"
    )
    lambda.t <- lambda1.t * latent.vec + lambda0 * (1 - latent.vec)
    return(lambda.t)
  } else if (method == "atevent") {
    latent_z <- event$z
    if (t[1] == 0) {
      t <- t[-1]
    }
    if (length(latent_z) == (length(t) + 1)) {
      latent_z <- latent_z[-1]
    }
    lambda.t <- rep(lambda0, length(t))
    r <- 0
    for (i in c(1:length(t))) {
      if (i > 1) {
        r <- exp(-beta * (t[i] - t[i - 1])) * (1 + r)
      }
      if (latent_z[i] == 1) {
        lambda.t[i] <- lambda1 + alpha * r
      }
    }
    return(lambda.t)
  }
}
