#' Create a Markov-modulated Hawkes Process(MMHP) model
#'
#' Create a Markov-modulated Hawkes Process(MMHP) model according to the given parameters: lambda0, lambda1, alpha, beta, event times and transition probability matrix。
#' If event time tau is missing, then it means that data will be added later(e.g. simulated)
#'
#' @param Q transition probability matrix.
#' @param tau vector containing the event times. Note that the first event is at time zero. Alternatively, tau could be specified as NULL, meaning that the data will be added later (e.g. simulated).
#' @param lambda0 parameters for homogeneous Poisson process.
#' @param lambda1 parameters for Hawkes process.
#' @param beta parameters for Hawkes process.
#' @param alpha parameters for Hawkes process.
#' @param delta initial state probability.
#'
#' @return mmhp object
#' @export
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1, alpha = 0.8, beta = 1.2)
mmhp <- function(Q, delta, tau = NULL, lambda0, lambda1, alpha, beta) {
  y <- c(list(Q = Q, delta = delta, tau = tau, lambda0 = lambda0, lambda1 = lambda1, alpha = alpha, beta = beta))
  class(y) <- "mmhp"
  return(y)
}
