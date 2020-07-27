#' Create a homogeneous poisson process model
#'
#' Create a homogeneous poisson process according to given parameters: lambda, start, end, and n.
#'
#' @param lambda rate of the poisson process
#' @param n number of events
#' @param start start time of events
#' @param end end time of events
#'
#' @return hpp object
#' @export
#' @examples
#' hpp(lambda = 1, end = 10)

hpp <- function(lambda, start=0, end=1, n = NULL) {
  y <- c(list(lambda = lambda, start = start, end = end, n = n, t=t))
  class(y) <- "hpp"
  return(y)
}
