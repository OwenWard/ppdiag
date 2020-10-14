#' Compute raw residuals for social network models
#'
#' Compute raw residuals for social network models with model
#'  specified time events or simulated time events
#'
#' @param object social network model containing the parameters
#' @param events vector of event happening time
#' @param start start of observation period
#' @param end end of observation period
#'
#' @return the raw residual
#' @export
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, 
#' lambda1 = 1.1, alpha = 0.8, beta = 1.2)
#' y <- simulatemmhp(x, n = 10)
#' rawresidual(x, events = y$events[-1])

rawresidual <- function(object, events, start, end) {
  UseMethod("rawresidual")
}

#' @rdname rawresidual
#' @export
rawresidual.default <- function(object, events, start = 0,
                                end = max(events)) {
  cat("Please input the right model. Select from hp, hpp and mmhp.")
}

#' @rdname rawresidual
#' @export
rawresidual.hp <- function(object, events, start = 0, 
                           end = max(events)) {
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

#' @rdname rawresidual
#' @export
rawresidual.mmhp <- function(object, events, start = 0,
                             end = max(events)) {
  event_obj <- list()
  event_obj$start <- start
  event_obj$end <- end
  event_obj$events <- events
  time.vec <- seq(from = start, to = end, length.out = 1000)
  N <- length(events)
  est.intensity <- intensity(object, event = event_obj, method = "numeric")
  # is this next line correct?
  all_Lambda <- sum(est.intensity) * (time.vec[2] - time.vec[1])
  return(N - all_Lambda)
}

#' @rdname rawresidual
#' @export
rawresidual.hpp <- function(object, events, start = 0, 
                            end = max(events)) {
  N <- length(events)
  inten_obj <- list(events = events, start = start, end = end)
  est.intensity <- intensity(object, events, method = "integral")
  all_Lambda <- sum(est.intensity)
  return(N - all_Lambda)
}
