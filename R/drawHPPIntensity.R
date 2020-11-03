#' Draw Intensity of Homogeneous Poisson Process 
#'
#' Draw the intensity for homogeneous poisson process events
#'
#' @param hpp object for homogeneous poisson process
#' @param events event times input
#' @param color a specification for the default plotting color.
#' @param plot_events a boolean indicating whether events 
#' inputted will be plotted
#' @param fit a boolean indicating whether to fit a hpp or
#'  use the passed object
#' @param int_title the plot title
#' @param start start of events
#' @param end end of events
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics points
#' @export
#' @examples
#' \dontrun{
#' pois_y <- hpp(lambda = 1)
#' drawHPPIntensity(pois_y, events=simulatehpp(pois_y, end=10), color = "red")
#' }
drawHPPIntensity <- function(hpp, events, color = "red", 
                             plot_events = FALSE,
                             fit = FALSE,
                             int_title = 
                               "Intensity homogeneous Poisson Process", start = 0, end = max(events)){
	old_events <- hpp$events
	
	if(is.null(old_events)){
	  if(fit==TRUE){
	    message("Fitting provided events.")
	    hpp_obj <- fithpp(events)
	    lambda <- hpp_obj$lambda
	    n <- hpp_obj$n
	  }else{
	    message("Using the hpp object. Set fit=TRUE to fit events. ")
	    lambda=hpp$lambda
	    n <- hpp$n
	  }
	}else if(!is.null(old_events) && !all(events==old_events)){
	  if(fit==TRUE){
	    message("Events in object and events provided don't match. Fitting provided events.")
	    hpp_obj <- fithpp(events)
	    lambda <- hpp_obj$lambda
	    n <- hpp_obj$n
	  }else{
	    message("Events in object and events provided don't match. Using the object. ")
	    lambda=hpp$lambda
	    events <- old_events
	    end <- max(events)
	    n <- hpp$n
	  }
	}else{
	  lambda <- hpp$lambda
	  n <- hpp$n
	}
	
	fisher <- 1/lambda
	plot(c(start,end), c(0,(lambda+fisher)*2), type = "n",
	     xlab = "event times", ylab = "lambda", 
		main=int_title)
	abline(h=lambda, col=color)
	abline(h=lambda+fisher,lty=2)	
	abline(h=lambda-fisher,lty=2)	
	if(plot_events==TRUE){
	  for(i in seq_along(events)){
	    points(x=events[i],y=0,pch=1,col="blue")
	  }
	}
	legend("topleft", "Events", col = "blue", pch = 1)
}
