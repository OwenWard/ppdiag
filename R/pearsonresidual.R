#' Compute Pearson residuals for point process models
#'
#' Compute Pearson residuals for point processes 
#' with specified parameters and events.
#'
#' @param object social network model containing the parameters
#' @param events vector of event times
#' @param start start of observation period (default 0)
#' @param end termination time (default final event)
#' 
#' @return the Pearson residual
#' @importFrom stats integrate
#' @export
#' @examples 
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, 
#' lambda1 = 1.1, alpha = 0.8, beta = 1.2)
#' y <- simulatemmhp(x, n = 10)
#' pearsonresidual(x, events = y$events[-1])

pearsonresidual <- function(object, events, start, end) {
  UseMethod("pearsonresidual")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.default <- function(object, events, start = 0,
                                end = max(events)) {
  cat("Please input the right model. Select from hp, hpp and mmhp. ")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.mmhp <- function(object, events, start = 0,
                                end = max(events)) {
  # define time.vec,latent.vec,latent_event in intensity
  N <- length(events)
  event_obj <- list()
  event_obj$events <- events
  event_obj$start <- start
  event_obj$end <- end
  time.vec <- seq(from = start, to = end, length.out = 1000)
  est.intensity <- intensity(object,event = event_obj,method = "numeric")
  est.intensity.events <- intensity(object, event = event_obj,
                                    method = "atevent")
  pr <- sum(1 / sqrt(est.intensity.events)) -
    sum(sqrt(est.intensity)) * (time.vec[2] - time.vec[1])
  return(pr)
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.hp <- function(object, events, start = 0,
                                end = max(events)) {
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta

  N <- length(events)
  PR <- 0
  r <- 0

  if (N == 0) {
    return(-sqrt(lambda0) * (end - start))
  } else if (N == 1) {
    PR <- PR + 1 / sqrt(lambda0) - sqrt(lambda0) * (events[1] - start)
    integrand <- function(u) {
      sqrt(lambda0 + alpha * exp(-beta * (u - events[1])))
    }
    PR <- PR - integrate(integrand, lower = events[1],
                         upper = end)$value
  } else {
    # first event
    PR <- PR + 1 / sqrt(lambda0) - sqrt(lambda0) * (events[1] - start)
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

      PR <- PR - integrate(integrand, lower = events[i - 1],
                           upper = events[i])$value
    }
    # N event ~ termination time

    integrand <- function(u) {
      temp <- lambda0
      for (k in c(1:N)) {
        temp <- temp + alpha * exp(-beta * (u - events[k]))
      }
      return(sqrt(temp))
    }

    PR <- PR - integrate(integrand, lower = events[N],
                         upper = end)$value
    return(PR)
  }
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.hpp <- function(object, events, start = 0,
                                end = max(events)) {
  if(end != max(events)) {
    message("PR calculated to specified end time.")
  }
  est.intensity <- sqrt(object$lambda)*(end-start)
  ### this pr looks incorrect to me
  N <- length(events)
  int_events <- rep(object$lambda,N)
  pr <- sum(1 / sqrt(int_events)) - est.intensity
  return(pr)
}
