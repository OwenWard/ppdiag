#' Draw Intensity of Homogeneous Poisson Process 
#'
#' Draw the intensity for homogeneous poisson process events
#'
#' @param hpp object for homogeneous poisson process
#' @param color a specification for the default plotting color.
#' @importFrom graphics plot
#' @importFrom graphics abline

#' @export


drawHPPIntensity <- function(hpp,color){
	start=hpp$start
	end=hpp$end
	lambda=hpp$lambda
	n=hpp$n
	if(!is.null(n)){
		events=simulatehpp(lambda=lambda, start=start, end=end, n=n) #if n not null, we need to simulate to get end
		end=max(events)
	}
	plot(c(start,end), c(0,lambda*2), type = "n", xlab = "event times", ylab = "lambda", 
		main="intensity of homogeneous poisson process")
	abline(h=lambda, col=color)
}