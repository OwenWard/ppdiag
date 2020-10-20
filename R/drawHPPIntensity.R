#' Draw Intensity of Homogeneous Poisson Process 
#'
#' Draw the intensity for homogeneous poisson process events
#'
#' @param hpp object for homogeneous poisson process
#' @param events event times input
#' @param color a specification for the default plotting color.
#' @param start start of events
#' @param end end of events
#' @param fit boolean indicating whether to fit hpp object
#' @param int_title the plot title
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics points
#' @export
#' @examples
#' \dontrun{
#' pois_y <- hpp(lambda = 1)
#' drawHPPIntensity(pois_y, events=simulatehpp(pois_y, end=10), color = "red")
#' }
drawHPPIntensity <- function(hpp, events, color = "red", start = 0, end = max(events), fit=FALSE, 
                             int_title = "Intensity homogeneous Poisson Process"){
	lambda <- hpp$lambda
	n <- hpp$n
	old_events <- hpp$events
	if(is.null(old_events)){
    message("No events in object. Plotting HPP with provided events.")
	}
	else if(!is.null(old_events) && !all(events==old_events)){
      message("Events in object and events provided don't match. Using provided events.")
	}
	if(fit==TRUE){
	  hpp_obj <- fithpp(events)
	  lambda <- hpp_obj$lambda
	  n <- hpp_obj$n
	}
	fisher <- 1/lambda
	plot(c(start,end), c(0,(lambda+fisher)*2), type = "n",
	     xlab = "event times", ylab = "lambda", 
		main=int_title)
	abline(h=lambda, col=color)
	abline(h=lambda+fisher,lty=2)	
	abline(h=lambda-fisher,lty=2)	
	for(i in seq_along(events)){
	  points(x=events[i],y=0,pch=1,col="blue")
	}
	legend("topleft", "Events", col = "blue", pch = 1)
}
