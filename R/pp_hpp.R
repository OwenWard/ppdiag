#' Create a homogeneous Poisson process object
#'
#' Create a homogeneous Poisson object according to given parameters:
#' lambda, and events.
#' If events are missing, then it means that data will be
#' added later(simulated from this process).
#' @param lambda rate of the Poisson process
#' @param events event times, optional
#'
#' @return hpp object
#' @export
#' @examples
#' pp_hpp(lambda = 1)
pp_hpp <- function(lambda, events = NULL) {
  y <- c(list(lambda = lambda, events = events))
  class(y) <- "hpp"
  return(y)
}
