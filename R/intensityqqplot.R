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
#' @importFrom graphics par
#' @importFrom graphics layout
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
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),heights = c(2, 2),widths = c(2, 2))
  r=compensator(object=object,t=events,pzt=pzt)  
  par(mar = c(2, 2,1,1))
  qqexp(r)
  par(mar = c(2, 2,1,1))
  drawHPIntensity(object,start=start,end=end,
                  history=history,events = events,color=color,i=i,add=FALSE)
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.hpp <- function(object, 
                            start, end, history,
                            color=1, i=1, events, pzt = NULL) {
	
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),heights = c(2, 2),widths = c(2, 2))
  r=compensator(object=object,t=events,pzt=pzt)  
  par(mar = c(2, 2,1,1))
  qqexp(r)  
  par(mar = c(2, 2,1,1))
  drawHPPIntensity(object, events = events, color = color,
                   plot_events = TRUE)  
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.mmhp <- function(object, 
                            start, end, history,
                            color=1, i=1, events, pzt) {
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),heights = c(2, 2),widths = c(2, 2))
  r=compensator(object=object,t=events,pzt=pzt)  
  par(mar = c(2, 2,1,1))
  qqexp(r)  
  par(mar = c(2, 2,1,1))
  drawUniMMHPIntensity(mmhp = object, simulation=events, 
                       add=FALSE, color=color,
                       given_main = "Intensity Plot of MMHP")  
}


