#' Draw Intensity of Homogeneous Poisson Process 
#'
#' Draw the intensity for homogeneous poisson process events
#'
#' @param hpp object for homogeneous poisson process
#' @param events event times input
#' @param color a specification for the default plotting color.
#' @param plot_events a boolean indicating whether events inputted will be plotted
#' @param fit a boolean indicating whether to fit a hpp or use the passed object
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics points
#' @export
#' @examples
#' pois_y <- hpp(lambda = 1, end = 10)
#' drawHPPIntensity(pois_y,color = "red")

drawHPPIntensity <- function(hpp, events = NULL, color = "red", 
                             plot_events = FALSE, fit = FALSE){
	start=hpp$start
	end=hpp$end
	lambda=hpp$lambda
	n=hpp$n
	if(!is.null(events)){
		if(plot_events==TRUE & fit == TRUE){
			message("Fitting and plotting a HPP. Specified fit not used.")
			hpp_obj=fithpp(events)
			start=hpp_obj$start
			end=hpp_obj$end
			lambda=hpp_obj$lambda
			n=hpp_obj$n
		}
	  if(plot_events == TRUE & fit == FALSE){
	    message("Fitting specified hpp.")
	  }
	  else{
		  message("The inputted events not used, hpp object and simulated events will be plotted.")
		  events=simulatehpp(hpp)
		}		
	}
	else if(!is.null(n)){
		events=simulatehpp(hpp) #if n not null, we need to simulate to get end
		end=max(events)
	}
	else{
	  events=simulatehpp(hpp)
	}
	fisher=1/lambda
	plot(c(start,end), c(0,(lambda+fisher)*2), type = "n", xlab = "event times", ylab = "lambda", 
		main="Intensity of homogeneous poisson process")
	abline(h=lambda, col=color)
	abline(h=lambda+fisher,lty=2)	
	abline(h=lambda-fisher,lty=2)	
	for(i in 1:length(events)){
	  points(x=events[i],y=0,pch=1,col="blue")
	}
	legend("topleft", "Homogeneous Poisson Process events", col = "blue", pch = 1)
}
