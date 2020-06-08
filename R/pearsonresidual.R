#' Compute Pearson residuals for social network models
#'
#' Compute Pearson residuals for social network models with model specified time events or simulated time events
#'
#' @param object social network model containing the parameters
#' @param t vector of event happening time
#' @param termination termination time
#' @param time.vec time segment to calculate the intensity for `numeric` method
#' @param latent.vec the probability of the latent space being in the active state
#' @param latent_event the estimated latent space of the point process model
#'
#' @return the pearson residual
#' @importFrom stats integrate
#' @export

pearsonresidual <- function(object, t, termination, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  UseMethod("pearsonresidual")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.default <- function(object, t, termination, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  cat("please input the right model")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.mmhp <- function(object, t, termination, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  N <- length(t)
  est.intensity <- intensity(object,
    event = list(
      tau = t,
      time_segment = time.vec,
      latent_mean = latent.vec
    ),
    method = "numeric"
  )
  est.intensity.events <- intensity(object, event = list(
    tau = t,
    z = latent_event
  ), method = "atevent")
  pr <- sum(1 / sqrt(est.intensity.events)) -
    sum(sqrt(est.intensity)) * (time.vec[2] - time.vec[1])
  return(pr)
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.hp <- function(object, t, termination, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta

  N <- length(t)
  PR <- 0
  r <- 0

  if (N == 0) {
    return(-sqrt(lambda0) * termination)
  } else if (N == 1) {
    PR <- PR + 1 / sqrt(lambda0) - sqrt(lambda0) * t[1]
    integrand <- function(u) {
      sqrt(lambda0 + alpha * exp(-beta * (u - t[1])))
    }
    PR <- PR - integrate(integrand, lower = t[1], upper = termination)$value
  } else {
    # first event
    PR <- PR + 1 / sqrt(lambda0) - sqrt(lambda0) * t[1]
    # 2~N t
    for (i in 2:N) {
      r <- exp(-beta * (t[i] - t[i - 1])) * (r + 1)
      if (lambda0 + alpha * r > 0) {
        PR <- PR + 1 / sqrt(lambda0 + alpha * r)
      }

      integrand <- function(u) {
        temp <- lambda0
        for (k in c(1:(i - 1))) {
          temp <- temp + alpha * exp(-beta * (u - t[k]))
        }
        return(sqrt(temp))
      }

      PR <- PR - integrate(integrand, lower = t[i - 1], upper = t[i])$value
    }
    # N event ~ termination time

    integrand <- function(u) {
      temp <- lambda0
      for (k in c(1:N)) {
        temp <- temp + alpha * exp(-beta * (u - t[k]))
      }
      return(sqrt(temp))
    }

    PR <- PR - integrate(integrand, lower = t[N], upper = termination)$value
    return(PR)
  }
}
