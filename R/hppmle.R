#' Fit a homogeneous poisson process
#'
#' Compute maximum likelihood estimator for the hpp model with time events 
#' 
#' @param t vector containing the event times.
#'
#' @return a scalar indicating the maximum likelihood estimator
#' @export

hppmle <- function(t){
	return (length(t)/(max(t)-min(t)))
}
