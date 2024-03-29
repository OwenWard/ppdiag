#' max intensity of a hawkes process
#'
#' @param object hawkes process object containing parameters for Hawkes process.
#' @param events events for Hawkes process.
#'
#' @return max of intensity
#' @keywords internal
#'
hawkes_max_intensity <- function(object, events) {
  alpha <- object$alpha
  beta <- object$beta
  if (alpha >= beta) {
    stop("A stationary Hawkes process requires alpha<beta.")
  }
  r <- 0
  r.max <- 0
  N <- length(events)
  i <- 2
  while (i <= N) {
    r <- exp(-1 * object$beta * (events[i] - events[i - 1])) * (1 + r)
    if (r > r.max) {
      r.max <- r
    }
    i <- i + 1
  }
  yupper <- object$lambda0 + object$alpha * r.max + object$alpha
  return(yupper)
}
