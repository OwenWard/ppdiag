#' Compute raw residuals for social network models
#'
#' Compute raw residuals for social network models with model
#'  specified time events or simulated time events
#'
#' @param object social network model contating the parameters
#' @param events vector of event happening time
#' @param start start of observation period
#' @param termination end of observation period
#'
#' @return the raw residual
#' @export

rawresidual <- function(object, events, start = 0, termination) {
  UseMethod("rawresidual")
}

#' @rdname rawresidual
#' @export
rawresidual.default <- function(object, events, start = 0, termination) {
  cat("Please input the right model. Select from hp, hpp and mmhp. ")
}

#' @rdname rawresidual
#' @export
rawresidual.hp <- function(object, events, start = 0, termination) {
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  hawkes_obj <- object
  event_obj <- list(events = events,
                    start = start,
                    termination = termination)
  N <- length(events)
  result <- intensity(hawkes_obj,event = event_obj, method = "integral")
  return(N - result)
}

#' @rdname rawresidual
#' @export
rawresidual.mmhp <- function(object, events, start = 0, termination) {
  ## need to define and compute time.vec, latent.vec in here
  # time.vec <- NA
  # latent.vec <- NA
  # actually only need to compute the latent vec here?
  event_obj <- list()
  event_obj$start <- start
  event_obj$termination <- termination
  event_obj$events <- events
  
  # temp definition to avoid issues checking
  time.vec <- seq(from = start, to = termination, length.out = 1000)
  
  N <- length(events)
  est.intensity <- intensity(object, event = event_obj, method = "numeric")
  # is this next line correct?
  all_Lambda <- sum(est.intensity) * (time.vec[2] - time.vec[1])
  return(N - all_Lambda)
}

#' @rdname rawresidual
#' @export
rawresidual.hpp <- function(object, events, start = 0, termination) {
  N <- length(events)
  inten_obj <- list(events = events, start = start, termination = termination)
  est.intensity <- intensity(object, events, method = "integral")
  all_Lambda <- sum(est.intensity)
  return(N - all_Lambda)
}
