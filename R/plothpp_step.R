#' Draw Homogeneous Poisson Process 
#'
#' Draw the event time counts for homogeneous poisson process
#'
#' @param object parameters for homogeneous poisson process
#' @param color A specification for the default plotting color.
#' @importFrom graphics plot
#' @importFrom stats stepfun


#' @export


plothpp_step <- function(events, color="red"){
	count=0:length(events)
	plot(stepfun(x=events, y=count), main="Homogeneous Poisson Process",
       xlab="Event Time", ylab="Number of events", col=color)
}