#' Draw Homogeneous Poisson Process 
#'
#' Draw the event time counts for homogeneous poisson process
#'
#' @param events parameters for homogeneous poisson process
#' @param color A specification for the default plotting color.
#' @importFrom graphics plot
#' @importFrom stats stepfun
#' @export
#' @examples
#' pois_y <- hpp(lambda = 1)
#' events <- pp_simulate(pois_y, end = 10, n=50)
#' plothpp_step(events)


plothpp_step <- function(events, color="red"){
	count <- 0:length(events)
	plot(stepfun(x=events, y=count), main="Homogeneous Poisson Process",
       xlab="Event Time", ylab="Number of events", col=color)
}
