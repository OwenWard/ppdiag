#' Fit a homogeneous poisson process
#'
#' Compute maximum likelihood estimator for the hpp model with time events 
#' 
#' @param events vector containing the event times.
#'
#' @return an hpp object fitted
#' @export
#' @examples
#' pois_y <- hpp(lambda = 1, end = 10)
#' events <- simulatehpp(pois_y)
#' fithpp(events)

fithpp <- function(events){
	start <- min(events)
	end <- max(events)
	if(start==end){
	  stop("Start and end time are equal.")
	}
	n <- length(events)
	lambda <- n/(end-start)
	hpp_object <- list(lambda = lambda, start = start,
	                end = end, n = n,
	                events = events)
	class(hpp_object) <- "hpp"
	return(hpp_object)	
}
