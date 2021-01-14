#' Create a Markov-modulated Hawkes Process(MMHP) object
#'
#' Create a Markov-modulated Hawkes Process(MMHP) model 
#' according to the given parameters: lambda0, lambda1, 
#' alpha, beta, event times and transition probability matrix.
#' If event time events is missing,
#'  then it means that data will be added later(e.g. simulated)
#'
#' @param Q transition probability matrix.
#' @param events vector containing the event times. 
#' Note that the first event is at time zero. 
#' Alternatively, events could be specified as NULL,
#'  meaning that the data will be added later (e.g. simulated).
#' @param lambda0 intensity for homogeneous Poisson process.
#' @param lambda1 base intensity for Hawkes process.
#' @param beta exponential decrease of intensity in the hawkes process
#' @param alpha jump size of the increase in intensity in the hawkes process
#' @param delta initial state probability.
#'
#' @return mmhp object
#' @export
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1,
#'  alpha = 0.8, beta = 1.2)
pp_mmhp <- function(lambda0, lambda1, alpha, beta, Q = NULL,
                 delta = NULL, events = NULL) {
  y <- c(list(Q = Q, delta = delta,
              events = events, lambda0 = lambda0, 
              lambda1 = lambda1, alpha = alpha, beta = beta))
  class(y) <- "mmhp"
  if(alpha > beta) {
    stop("Require alpha less than beta for a stationary process")
  }
  return(y)
}
