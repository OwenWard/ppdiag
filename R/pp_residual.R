#' Compute raw and pearson residuals for point process models

#' @param object point process model containing the parameters
#' @param events vector of event times
#' @param start start of observation period (default 0)
#' @param end end of observation period (default final event)
#'
#' @return the raw and pearson residuals
#' @export
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, 
#' lambda1 = 1.1, alpha = 0.8, beta = 1.2)
#' y <- pp_simulate(x, n = 10)
#' pp_residual(x, events = y$events)

pp_residual <- function(object, events, start = 0, end = max(events)) {
  rr <- rawresidual(object, events, start, end)
  pr <- pearsonresidual(object, events, start, end)
  return(list(raw = rr, pearson = pr))
}