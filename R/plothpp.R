#' Draw Homogeneous Poisson Process 
#'
#' Draw the event time counts for homogeneous poisson process
#'
#' @param events homogeneous poisson process events
#' @param color a specification for the default plotting color.
#' @importFrom graphics plot

#' @export


plothpp <- function(events,color){
	count=0:length(events)
	plot(stepfun(x=events, y=count), main="Homogeneous Poisson Process",
       xlab="Event Time", ylab="Number of events", col=color)
}
