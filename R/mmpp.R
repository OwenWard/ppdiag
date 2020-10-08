#' Create a Markov-modulated Poisson Process(MMPP) model
#'
#' Create a Markov-modulated Poisson Process(MMPP) model
#'  according to the given parameters: lambda0, c, q1, q2 and event times.
#' If event time tau is missing, 
#' then it means that data will be added later(e.g. simulated)
#'
#' @param q1 transition probability
#' @param q2 transition probability
#' @param c the proportion of intensity 1 over intensity 2
#' @param tau vector containing the event times.
#'  Note that the first event is at time zero. 
#'  Alternatively, tau could be specified as NULL,
#'   meaning that the data will be added later (e.g. simulated).
#' @param lambda0 parameters for Poisson process.
#'
#' @return mmpp object
#' @export

mmpp <- function(lambda0, c, q1, q2, tau = NULL) {
  y <- c(list(c = c, tau = tau, lambda0 = lambda0, q1 = q1, q2 = q2))
  class(y) <- "mmpp"
  return(y)
}
