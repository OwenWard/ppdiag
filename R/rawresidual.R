#' Compute raw residuals for social network models
#'
#' Compute raw residuals for social network models with model specified time events or simulated time events
#'
#' @param object social network model contating the parameters
#' @param events vector of event happening time
#' @param termination termination time
#'
#' @return the raw residual
#' @export

rawresidual <- function(object, events, termination) {
  UseMethod("rawresidual")
}

#' @rdname rawresidual
#' @export
rawresidual.default <- function(object, events, termination) {
  cat("Please input the right model. Select from hp, hpp and mmhp. ")
}

#' @rdname rawresidual
#' @export
rawresidual.hp <- function(object, events, termination) {
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta

  N <- length(events)
  r <- 0

  if (N > 1) {
    for (i in 2:N) {
      r <- exp(-beta * (events[i] - events[i - 1])) * (r + 1)
    }
  }

  if (N == 0) {
    result <- lambda0 * termination
  } else {
    result <- lambda0 * termination + 
      alpha / beta * (N - (1 + r) * exp(-beta * (termination - events[N])))
  }

  return(N - result)
}

#' @rdname rawresidual
#' @export
rawresidual.mmhp <- function(object, events, termination) {
  ## need to define and compute time.vec, latent.vec in here
  time.vec <- NA
  latent.vec <- NA
  
  
  N <- length(events)
  est.intensity <- intensity(object,
    event = list(
      events = events,
      time_segment = time.vec,
      latent_mean = latent.vec
    ),
    method = "numeric"
  )
  all_Lambda <- sum(est.intensity) * (time.vec[2] - time.vec[1])
  return(N - all_Lambda)
}

#' @rdname rawresidual
#' @export
rawresidual.hpp <- function(object, events, termination) {
  N <- length(t)
  end <- object$end
  est.intensity <- intensity(object, events, method = "numeric")
  all_Lambda <- sum(est.intensity)
  return(N - all_Lambda)
}
