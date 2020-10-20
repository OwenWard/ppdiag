#' Create a homogeneous poisson process model
#'
#' Create a homogeneous poisson process according to given parameters:
#'  lambda, start, end, and n.
#'
#' @param lambda rate of the poisson process
#' @param n number of events
#' @param events event times
#'
#' @return hpp object
#' @export
#' @examples
#' hpp(lambda = 1)

hpp <- function(lambda, n = NULL, events = NULL) {
  y <- c(list(lambda = lambda, n = n, events = NULL))
  class(y) <- "hpp"
  return(y)
}
