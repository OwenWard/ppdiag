#' Draw intensity of homogeneous Poisson process 
#'
#' Draw the intensity for a homogeneous Poisson process
#'
#' @param hpp object for homogeneous poisson process
#' @param events event times input
#' @param color a specification for the default plotting color.
#' @param plot_events a boolean indicating whether events 
#' inputted will be plotted
#' @param fit a boolean indicating whether to fit a hpp or
#'  use the passed object
#'  @param add whether to add the hpp intensity to an existing plot
#' @param int_title the plot title
#' @param start start of events
#' @param end end of events
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics points
#' @export
#' @examples
#' \dontrun{
#' pois_y <- pp_hpp(lambda = 1)
#' drawHPPIntensity(pois_y, events=pp_simulate(pois_y, end=10), color = "red")
#' }
drawHPPIntensity <- function(hpp, events, color = "red", 
                             plot_events = FALSE,
                             fit = FALSE,
                             add = FALSE,
                             int_title = 
                               "Intensity homogeneous Poisson Process", start = 0, end = max(events)){
	old_events <- hpp$events
	if (add==FALSE){
	  if(is.null(old_events)){
	    if(is.null(events)){
	      stop("Events must be provided either in the object or in the events argument. ")
	    }
	    if(fit==TRUE){
	      message("Fitting provided events.")
	      hpp_obj <- fithpp(events)
	      lambda <- hpp_obj$lambda
	      n <- hpp_obj$n
	    }else{
	      message("Using the hpp object. Set fit=TRUE to fit events provided. ")
	      lambda=hpp$lambda
	      n <- hpp$n
	    }
	  }else{
	    if(is.null(events)){
	      message("No events provided. Using the hpp object.")
	      lambda <- hpp$lambda
	      n <- hpp$n
	      events <- hpp$events
	      if(start>0){
	        start <- min(events)
	      }
	      end <- max(events)
	    }else{
	      if(fit==TRUE){
	        message("Fitting provided events. Set events=NULL to use the events in object.")
	        hpp_obj <- fithpp(events)
	        lambda <- hpp_obj$lambda
	        n <- hpp_obj$n
	      }else{
	        message("Using the hpp object. Set fit=TRUE to fit events provided. ")
	        lambda <- hpp$lambda
	        n <- hpp$n
	      }
	    }
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
	
	
	
	else{
	  # to add to an already created plot
	  lambda <- hpp$lambda
	  # plot(c(start,end), c(0,(lambda+fisher)*2), type = "n",
	  #      xlab = "event times", ylab = "lambda", 
	  #      main=int_title)
	  graphics::segments(x0=start, x1=end, y0=lambda, col=color)
	  if(plot_events==TRUE){
	    for(i in seq_along(events)){
	      points(x=events[i],y=0,pch=1,col="blue")
	    }
	  }
	}

}
