#' Compute raw residuals for social network models
#'
#' Compute raw residuals for social network models with model specified time events or simulated time events
#'
#' @param object social network model contating the parameters
#' @param t vector of event happening time
#' @param termination termination time
#' @param time.vec time segment to calculate the intensity for `numeric` method
#' @param latent.vec the probability of the latent space being in the active state
#'
#' @return the raw residual
#' @export

rawresidual <- function(object, t, termination, time.vec, latent.vec) {
  UseMethod("rawresidual")
}

#' @rdname rawresidual
#' @export
residual.default <- function(object, t, termination, time.vec, latent.vec) {
  cat("please input the right model")
}

#' @rdname rawresidual
#' @export
rawresidual.hp <- function(object, t, termination, time.vec, latent.vec) {
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta

  N <- length(t)
  r <- 0

  if (N > 1) {
    for (i in 2:N) {
      r <- exp(-beta * (t[i] - t[i - 1])) * (r + 1)
    }
  }

  if (N == 0) {
    result <- lambda0 * termination
  } else {
    result <- lambda0 * termination + alpha / beta * (N - (1 + r) * exp(-beta * (termination - t[N])))
  }

  return(N - result)
}

#' @rdname rawresidual
#' @export
rawresidual.mmhp <- function(object, t, termination, time.vec, latent.vec) {
  N <- length(t)
  est.intensity <- intensity(object,
    event = list(
      tau = t,
      time_segment = time.vec,
      latent_mean = latent.vec
    ),
    method = "numeric"
  )
  all_Lambda <- sum(est.intensity) * (time.vec[2] - time.vec[1])
  return(N - all_Lambda)
}
