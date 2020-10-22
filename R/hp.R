#' Create a Hawkes Process(HP) model
#'
#' Create a Hawkes Process(HP) model according to the given parameters:
#'  lambda0, alpha, beta and events.
#' If events is missing, then it means that data will be
#'  added later(e.g. simulated)
#'
#' @param beta exponential decay of intensity
#' @param alpha jump size in increase of intensity
#' @param events vector containing the event times. 
#' Note that the first event is at time zero. 
#' Alternatively, events could be specified as NULL, 
#' meaning that the data will be added later (e.g. simulated).
#' @param lambda0 initial intensity at the start time
#'
#' @return hp object
#' @export
#' @examples
#' hp(lambda0 = 0.1,alpha = 0.45,beta = 0.5)
hp <- function(lambda0, alpha, beta, events = NULL) {
  if(alpha>=beta){
    stop("A stationary hawkes process requires alpha<beta.")
  }
  y <- c(list(lambda0 = lambda0, alpha = alpha, beta = beta,
              events = events))
  class(y) <- "hp"
  return(y)
}
