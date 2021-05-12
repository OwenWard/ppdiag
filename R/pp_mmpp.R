#' Create a Markov-modulated Poisson Process(MMPP) object
#'
#' Create a Markov-modulated Poisson Process(MMPP) model
#'  according to the given parameters: lambda0, c, q1, q2 and event times.
#' If event time tau is missing,
#' then it means that data will be added later(e.g. simulated)
#'
#' @param Q transition probability matrix
#' @param lambda0 parameters for Poisson process.
#' @param c the proportion of intensity 1 over intensity 2
#' @param events vector containing the event times.
#'  Note that the first event is often specified as zero.
#'  Alternatively, events could be specified as NULL,
#'   meaning that the data will be added later (e.g. simulated).
#' @param delta initial state probability.

#'
#' @return mmpp object
#' @export
#'
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' pp_mmpp(Q = Q, lambda0 = 1, c = 1.5, delta = c(1 / 3, 2 / 3))
pp_mmpp <- function(lambda0, c, Q, events = NULL, delta = NULL) {
  y <- c(list(
    c = c, events = events, lambda0 = lambda0, Q = Q,
    delta = delta
  ))
  class(y) <- "mmpp"
  return(y)
}

#' @export
print.mmpp <- function(x, ...) {
  cat("Markov Modulated Poisson Process \n")
  cat("lambda0 ", x$lambda0, "\n")
  cat("c ", x$c, "\n")
  cat("Q ", x$Q, "\n")
  if(!(is.null(x$delta))) {
    cat("delta", x$delta, "\n") 
  }
  if(!(is.null(x$events))) {
    cat("events", x$events, "\n") 
  }
  invisible(NULL)
}