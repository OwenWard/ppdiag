#' Create a Hawkes Process(HP) model
#'
#' Create a Hawkes Process(HP) model according to the given parameters: lambda0, alpha, beta and event times.
#' If event time tau is missing, then it means that data will be added later(e.g. simulated)
#'
#' @param beta parameters for Hawkes process.
#' @param alpha parameters for Hawkes process.
#' @param t vector containing the event times. Note that the first event is at time zero. Alternatively, t could be specified as NULL, meaning that the data will be added later (e.g. simulated).
#' @param lambda0 parameters for Hawkes process.
#'
#' @return hp object
#' @export
#' @examples
#' hp(lambda0 = 0.1,alpha = 0.45,beta = 0.5)
hp <- function(lambda0, alpha, beta, t = NULL) {
  if(alpha>=beta){
    stop("A stationary hawkes process requires alpha<beta.")
  }
  y <- c(list(lambda0 = lambda0, alpha = alpha, beta = beta, t = t))
  class(y) <- "hp"
  return(y)
}
