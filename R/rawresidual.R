#' Compute raw residuals for point process models
#'
#' Compute raw residuals for for point processes 
#' with specified parameters and events.
#'
#' @param object point process model containing the parameters
#' @param events vector of event times
#' @param start start of observation period (default 0)
#' @param end end of observation period (default final event)
#' @return the raw residual
#' @export
#' @examples 
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, 
#' lambda1 = 1.1, alpha = 0.8, beta = 1.2)
#' y <- pp_simulate(x, n = 10)
#' pearsonresidual(x, events = y$events[-1])





#' @export
rawresidual <- function(object, events, start, end) {
  UseMethod("rawresidual")
}

#' @export
rawresidual.default <- function(object, events, start = 0,
                                end = max(events)) {
  cat("Please input the right model. Select from hp, hpp and mmhp.")
}

#' @export
rawresidual.hp <- function(object, events, start = 0, 
                           end = max(events)) {
  if(end != max(events)) {
    message("PR calculated to specified end time.")
  }
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  hawkes_obj <- object
  event_obj <- list(events = events,
                    start = start,
                    end = end)
  N <- length(events)
  result <- intensity(hawkes_obj,event = event_obj, method = "integral")
  return(N - result)
}

#' @export
rawresidual.mmhp <- function(object, events, start = 0,
                             end = max(events)) {
  if(end != max(events)) {
    message("PR calculated to specified end time.")
  }
  event_obj <- list()
  event_obj$start <- start
  event_obj$end <- end
  event_obj$events <- events
  time.vec <- seq(from = start, to = end, length.out = 1000)
  N <- length(events)
  est.intensity <- intensity(object, event = event_obj, method = "numeric")
  all_Lambda <- sum(est.intensity) * (time.vec[2] - time.vec[1])
  return(N - all_Lambda)
}

#' @export
rawresidual.hpp <- function(object, events, start = 0, 
                            end = max(events)) {
  if(end != max(events)) {
    message("PR calculated to specified end time.")
  }
  N <- length(events)
  # est.intensity <- intensity(object, events, method = "integral")
  all_Lambda <- object$lambda*(end - start)
  return(N - all_Lambda)
}
