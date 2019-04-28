#' Create a Markov-modulated Hawkes Process(MMHP) model
#'
#' Create a Markov-modulated Hawkes Process(MMHP) model according to the given parameters: lambda0, lambda1, alpha, beta, event times and the states
#'
#' @param state different states with state = 1 corresponds to active state, the Markov process and state = 2 corresponds to inactive state, the homogeneous Poisson process
#' @param state_time time of each transition of Markov process
#' @param tau times of Poisson events
#' @param lambda0 parameters for homogeneous Poisson process
#' @param lambda1 parameters for Hawkes process
#' @param beta parameters for Hawkes process
#' @param alpha parameters for Hawkes process
#'
#' @return mmhp object
#' @export
#' @examples
#' mmhp(c(1,2),c(0,8.36),c(0,1.27),lambda0 = 0.9, lambda1 = 1.1, alpha = 0.8, beta = 1.2)
mmhp <- function (state, state_time, tau, lambda0, lambda1, alpha, beta) {
  #create zt to save the Markov Process state at time tau when event happens
  n <- length(tau)
  zt <- rep(state[1], n)
  j <- 1
  ns <- length(state)
  state_time[ns + 1] <- max(tau[n] + 1, state_time[ns] + 1)
  for (i in 1:n) {
    while (tau[i] > state_time[j+1]){
      j <- j + 1
    }
    zt[i] <- state[j]
  }
  #return mmhp object
  y <- c(list(tau = tau, state = state, state_time = state_time[1 : ns], tau_state = zt, lambda0 = lambda0, lambda1 = lambda1, alpha = alpha, beta = beta))
  class(y) <- "mmhp"
  return(y)
}
