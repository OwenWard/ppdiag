#' Draw Intensity of Homogeneous Poisson Process 
#'
#' Draw the intensity for homogeneous poisson process events
#'
#' @param hpp object for homogeneous poisson process
#' @param events event times input
#' @param color a specification for the default plotting color.
#' @param plot_events a boolean indicating whether events inputted will be plotted
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics points
#' @export


drawHPPIntensity <- function(hpp, events=NULL, color = "red", plot_events=FALSE){
	start=hpp$start
	end=hpp$end
	lambda=hpp$lambda
	n=hpp$n
	if(!is.null(events)){
		if(plot_events==TRUE){
			message("The inputted events and its corresponding intensity will be plotted, the hpp object input will be ignored.")
			hpp_obj=fithpp(events)
			start=hpp_obj$start
			end=hpp_obj$end
			lambda=hpp_obj$lambda
			n=hpp_obj$n
		}else{
		  message("The inputted events will be ignored, the hpp object and its simulated events will be plotted.")
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
	plot(c(start,end), c(0,lambda*2), type = "n", xlab = "event times", ylab = "lambda", 
		main="Intensity of homogeneous poisson process")
	abline(h=lambda, col=color)
	for(i in 1:length(events)){
	  points(x=events[i],y=0,pch=1,col="blue")
	}
	legend("topleft", "Homogeneous Poisson Process events", col = "blue", pch = 1)
}
