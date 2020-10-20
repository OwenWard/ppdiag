#' Create a homogeneous poisson process model
#'
#' Create a homogeneous poisson process according to given parameters:
#'  lambda, start, end, and n.
#'
#' @param lambda rate of the poisson process
#' @param events event times
#'
#' @return hpp object
#' @export
#' @examples
#' hpp(lambda = 1)

hpp <- function(lambda, events = NULL) {
  y <- c(list(lambda = lambda, events = NULL))
  class(y) <- "hpp"
  return(y)
}
