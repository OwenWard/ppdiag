#' Fit a homogeneous poisson process
#'
#' Compute maximum likelihood estimator for the hpp model with time events 
#' 
#' @param t vector containing the event times.
#'
#' @return an hpp object fitted
#' @export
#' @examples
#' pois_y <- hpp(lambda = 1, end = 10)
#' events <- simulatehpp(pois_y)
#' fithpp(events)

fithpp <- function(t){
	start=min(t)
	end=max(t)
	n=length(t)
	lambda=n/(end-start)
	hpp_object=list(lambda=lambda, start=start, end=end, n=n, t=t)
	class(hpp_object)="hpp"
	return(hpp_object)	
}
