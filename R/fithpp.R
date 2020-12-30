#' Fit a homogeneous poisson process to event data
#'
#' Compute maximum likelihood estimator of the rate of a homogeneous Poisson
#' process for the given events. 
#' 
#' @param events vector containing the event times.
#'
#' @return a hpp object containing the events and the estimated parameter 
#' @export
#' @examples
#' pois_y <- hpp(lambda = 1)
#' events <- pp_simulate(pois_y, end=10)
#' fithpp(events)

fithpp <- function(events){
	start <- min(events)
	end <- max(events)
	if(start==end){
	  stop("Start and end time are equal.")
	}
	n <- length(events)
	lambda <- n/(end-start)
	hpp_object <- list(lambda = lambda, events = events)
	class(hpp_object) <- "hpp"
	return(hpp_object)	
}
