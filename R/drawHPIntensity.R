#' Draw the intensity of Hawkes Process
#'
#' Draw the intensity of Hawkes Process, a helper function for 'drawUniMMHPIntensity'
#'
#' @param lambda0 parameters for Hawkes process
#' @param i state number
#' @param beta parameters for Hawkes process
#' @param alpha parameters for Hawkes process
#' @param start the start time of current state
#' @param end the end time of current state
#' @param history the past event times
#' @param hawkes_time the event times happened in this state
#' @param color A specification for the default plotting color.
#' @importFrom graphics curve
#' @importFrom graphics segments

#' @export


drawHPIntensity <- function(lambda0, i, alpha, beta, start, end, history, hawkes_time, color = 1, add=FALSE) {
  n <- length(hawkes_time)
  m <- length(history)

  if(add==FALSE){
    graphics::plot(0, 0,
      xlim = c(start, hawkes_time[n]), ylim = c(0, 1), type = "n", xlab = "Time", ylab = "Intensity",
      main = 'Hawkes Process'
    )
  }
  
  if (n == 0) {
    if (i == 1) {
      segments(x0 = start, x1 = end, y0 = lambda0)
    } else {
      lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m) - history)))
      new.lambda.n <- Vectorize(lambda.n)
      graphics::segments(x0 = start, y0 = lambda0, y1 = lambda.n(end), lty = 2, col = color)
      graphics::curve(new.lambda.n, from = start, to = end, add = TRUE, col = color)
    }
  } else {
    if (i == 1) {
      graphics::segments(x0 = start, x1 = hawkes_time[1], y0 = lambda0, col = color)
      segments(x0 = hawkes_time[1], y0 = lambda0, y1 = lambda0 + alpha, col = color)
    } else {
      lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m) - history)))
      new.lambda.n <- Vectorize(lambda.n)
      segments(x0 = start, y0 = lambda0, y1 = lambda.n(start), lty = 2, col = color)
      graphics::curve(new.lambda.n, from = start, to = hawkes_time[1], add = TRUE, col = color)
      segments(x0 = hawkes_time[1], y0 = lambda.n(hawkes_time[1]), y1 = lambda.n(hawkes_time[1]) + alpha, col = color)
    }
    if (n > 1) {
      for (j in 1:(n - 1)) {
        lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m + j) - c(history, hawkes_time[1:j]))))
        new.lambda.n <- Vectorize(lambda.n)
        curve(new.lambda.n, from = hawkes_time[j], to = hawkes_time[j + 1], add = TRUE, col = color)
        segments(x0 = hawkes_time[j + 1], y0 = lambda.n(hawkes_time[j + 1]), y1 = lambda.n(hawkes_time[j + 1]) + alpha, col = color)
      }
    }
    lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m + n) - c(history, hawkes_time[1:n]))))
    new.lambda.n <- Vectorize(lambda.n)
    curve(new.lambda.n, from = hawkes_time[n], to = end, add = TRUE, col = color)
    segments(x0 = end, y0 = lambda.n(end), y1 = lambda0, lty = 2, col = color)
  }
}
