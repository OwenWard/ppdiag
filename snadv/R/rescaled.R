#' Generate the rescaled-inter-event-times for point process models
#'
#' Generate the rescaled-inter-event-times for point process models in order to implement time rescaling theorem and for model checking
#'
#' @param lambda intensity function of point process models
#' @param tau event times

#' @export



rescaled <- function(lambda,tau){
  rescaled_tau <- vector()
  rescaled_tau[1] <- 0
  m <- length(tau)
  for(i in 1 : (m - 1)){
    rescaled_tau[i+1]<-stats::integrate(lambda,tau[i],tau[i+1])$value
  }
  return(rescaled_tau)
}
