#' Draw the intensity and q-q plot
#'
#' Draw the intensity and q-q plot for models
#'
#' @param object parameters for the models: hp, hpp, and mmhp
#' @param start the start time of current state
#' @param end the end time of current state
#' @param history the past event times
#' @param color A specification for the default plotting color for intensity plot.
#' @param i state number this corresponds to a state jump directly before
#' @param events event times
#' @param pzt probability for calculating rescaled-interevent times.
#' which is only important when using mmhp


#' @export
intensityqqplot <- function(object, start, end, history, color, i, events, pzt){
	UseMethod("intensityqqplot")
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.default <- function(object, 
                            start, end, history,
                            color, i, events, pzt) {
  cat("Please input the right model. Select from hp, hpp, and mmhp. ")   
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.hp <- function(object, 
                            start, end, history,
                            color=1, i=1, events, pzt = NULL) {
  r=compensator(object=object,t=events,pzt=pzt)  
  par(mfrow=c(1,2))
  qqexp(r)
  drawHPIntensity(object=object,start=start,end=end,history=history,hawkes_time=events,color=color,i=i,add=FALSE)
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.hpp <- function(object, 
                            start, end, history,
                            color=1, i=1, events, pzt = NULL) {
  r=compensator(object=object,t=events,pzt=pzt)  
  par(mfrow=c(1,2))
  qqexp(r)  
  drawHPPIntensity(object, events=events, color = color)  
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.mmhp <- function(object, 
                            start, end, history,
                            color=1, i=1, events, pzt) {
  r=compensator(object=object,t=events,pzt=pzt)  
  par(mfrow=c(1,2))
  qqexp(r)  
  simulation=simulatemmhp(object)
  drawUniMMHPIntensity(object=object, simulation=simulation, add=FALSE, color=color, given_main = "Intensity Plot of MMHP")  
}


