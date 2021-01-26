#' Create a Markov-modulated Poisson Process(MMPP) object
#'
#' Create a Markov-modulated Poisson Process(MMPP) model
#'  according to the given parameters: lambda0, c, q1, q2 and event times.
#' If event time tau is missing, 
#' then it means that data will be added later(e.g. simulated)
#'
#' @param q1 transition probability
#' @param q2 transition probability
#' @param c the proportion of intensity 1 over intensity 2
#' @param events vector containing the event times.
#'  Note that the first event is often specified as zero. 
#'  Alternatively, events could be specified as NULL,
#'   meaning that the data will be added later (e.g. simulated).
#' @param lambda0 parameters for Poisson process.
#'
#' @return mmpp object
#' @export
#' 
#' @examples 
#' pp_mmpp(lambda0 = 1, c = 1.5, q1 = 0.4, q2 = 0.2)

pp_mmpp <- function(lambda0, c, q1, q2, events = NULL) {
  y <- c(list(c = c, events = events, lambda0 = lambda0, q1 = q1, q2 = q2))
  class(y) <- "mmpp"
  return(y)
}
