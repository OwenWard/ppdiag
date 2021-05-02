#' Compute raw residuals for point process models
#'
#' Compute raw residuals for for point processes 
#' with specified parameters and events.
#'
#' @param object point process model containing the parameters
#' @param events vector of event times
#' @param start start of observation period (default 0)
#' @param end end of observation period (default final event)
#' @param steps number of steps for numeric integration (if needed)
#' @return the raw residual
#' @keywords internal
#' @examples 
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, 
#' lambda1 = 1.1, alpha = 0.8, beta = 1.2)
#' y <- pp_simulate(x, n = 10)
#' pearsonresidual(x, events = y$events[-1])






rawresidual <- function(object, events, start, end, steps = 1000) {
  UseMethod("rawresidual")
}


rawresidual.default <- function(object, events, start = 0,
                                end = max(events), steps) {
  cat("Please input the right model. Select from hp, hpp and mmhp.")
}


rawresidual.hp <- function(object, events, start = 0, 
                           end = max(events), steps = 1000) {
  if(end != max(events)) {
    message("RR calculated to specified end time.")
  }
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  hawkes_obj <- object
  if(events[1] == 0) {
    events <- events[-1]
  }
  event_obj <- list(events = events,
                    start = start,
                    end = end)
  N <- length(events)
  result <- pp_intensity(hawkes_obj,
                         event_info = event_obj,
                         method = "integral")
  return(N - result)
}


rawresidual.mmhp <- function(object, events, start = 0,
                             end = max(events), steps = 1000) {
  if(end != max(events)) {
    message("RR calculated to specified end time.")
  }
  if(events[1] == 0) {
    events <- events[-1]
  }
  event_obj <- list()
  event_obj$start <- start
  event_obj$end <- end
  event_obj$events <- events
  N <- length(events)
  est.intensity <- pp_intensity(object, event_info = event_obj,
                             method = "numeric", steps = steps)
  time.vec <- seq(from = start, to = end, length.out = steps)
  all_Lambda <- sum(est.intensity) * (time.vec[2] - time.vec[1])
  return(N - all_Lambda)
}


rawresidual.hpp <- function(object, events, start = 0, 
                            end = max(events), steps = 1000) {
  if(events[1] == 0){
    events <- events[-1]
  }
  if(end != max(events)) {
    message("RR calculated to specified end time.")
  }
  N <- length(events)
  # est.intensity <- intensity(object, events, method = "integral")
  all_Lambda <- object$lambda*(end - start)
  return(N - all_Lambda)
}


rawresidual.mmpp <- function(object, events, start = 0,
                             end = max(events), steps = 1000) {
  if(end != max(events)) {
    message("RR calculated to specified end time.")
  }
  if( events[1] == 0) {
    events <- events[-1]
  }
  N <- length(events)
  event_obj <- list()
  event_obj$start <- start
  event_obj$end <- end
  event_obj$events <- events
  est.intensity <- pp_intensity(object, event_info = event_obj,
                             method = "numeric", steps = steps)
  time.vec <- seq(from = start, to = end, length.out = steps)
  all_Lambda <- sum(est.intensity) * (time.vec[2] - time.vec[1])
  return(N - all_Lambda)
}
  
