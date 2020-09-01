#' Compute Pearson residuals for social network models
#'
#' Compute Pearson residuals for social network models with model specified time events or simulated time events
#'
#' @param object social network model containing the parameters
#' @param events vector of event happening time
#' @param termination termination time
#' 
#' @return the Pearson residual
#' @importFrom stats integrate
#' @export

pearsonresidual <- function(object, events, termination) {
  UseMethod("pearsonresidual")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.default <- function(object, events, termination) {
  cat("Please input the right model. Select from hp, hpp and mmhp. ")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.mmhp <- function(object, events, termination) {
  # define time.vec,latent.vec,latent_event in here
  time.vec <- NA
  latent.vec <- NA
  latent_event <- NA
  
  N <- length(events)
  est.intensity <- intensity(object,
    event = list(
      events = events,
      time_segment = time.vec,
      latent_mean = latent.vec
    ),
    method = "numeric"
  )
  est.intensity.events <- intensity(object, event = list(
    events = events,
    z = latent_event
  ), method = "atevent")
  pr <- sum(1 / sqrt(est.intensity.events)) -
    sum(sqrt(est.intensity)) * (time.vec[2] - time.vec[1])
  return(pr)
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.hp <- function(object, events, termination) {
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta

  N <- length(events)
  PR <- 0
  r <- 0

  if (N == 0) {
    return(-sqrt(lambda0) * termination)
  } else if (N == 1) {
    PR <- PR + 1 / sqrt(lambda0) - sqrt(lambda0) * events[1]
    integrand <- function(u) {
      sqrt(lambda0 + alpha * exp(-beta * (u - events[1])))
    }
    PR <- PR - integrate(integrand, lower = events[1], upper = termination)$value
  } else {
    # first event
    PR <- PR + 1 / sqrt(lambda0) - sqrt(lambda0) * events[1]
    # 2~N t
    for (i in 2:N) {
      r <- exp(-beta * (events[i] - events[i - 1])) * (r + 1)
      if (lambda0 + alpha * r > 0) {
        PR <- PR + 1 / sqrt(lambda0 + alpha * r)
      }

      integrand <- function(u) {
        temp <- lambda0
        for (k in c(1:(i - 1))) {
          temp <- temp + alpha * exp(-beta * (u - events[k]))
        }
        return(sqrt(temp))
      }

      PR <- PR - integrate(integrand, lower = events[i - 1], upper = events[i])$value
    }
    # N event ~ termination time

    integrand <- function(u) {
      temp <- lambda0
      for (k in c(1:N)) {
        temp <- temp + alpha * exp(-beta * (u - events[k]))
      }
      return(sqrt(temp))
    }

    PR <- PR - integrate(integrand, lower = events[N], upper = termination)$value
    return(PR)
  }
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.hpp <- function(object, events, termination=max(events)) {
  if((!is.null(termination)) && (termination!=object$end)){
    message("PR calculated to specified end time.")
    object$end=termination
  }
  est.intensity <- intensity(object, events, method = "numeric")
  pr <- sum(1 / sqrt(est.intensity)) - sum(sqrt(est.intensity))
  return(pr)
}
