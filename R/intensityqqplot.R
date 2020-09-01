#' Draw the intensity and q-q plot
#'
#' Draw the intensity and q-q plot for models
#'
#' @param object parameters for the models: hp, hpp, and mmhp
#' @param events event times
#' @param ... further arguments
#' @param pzt probability for calculating rescaled-interevent times.
#' which is only important when using mmhp
#' @importFrom graphics par
#' @importFrom graphics layout
#' @export
intensityqqplot <- function(object, events, pzt, ...){
	UseMethod("intensityqqplot")
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.default <- function(object, events, pzt, ...) {
  cat("Please input the right model. Select from hp, hpp, and mmhp. ")   
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.hp <- function(object, events, pzt=NULL, ...) {
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),heights = c(2, 2),
         widths = c(2, 2))
  r=compensator(object = object, events = events, pzt = pzt)  
  par(mar = c(2, 2,1,1))
  qqexp(r)
  par(mar = c(2, 2,1,1))
  drawHPIntensity(object,events = events,add=FALSE, ...)
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.hpp <- function(object, events, pzt, ...) {
	
  layout(mat = matrix(c(1,2), nrow = 1, ncol = 2),
         heights = c(2, 2), widths = c(2, 2))
  r=compensator(object = object, events = events, pzt = pzt)  
  par(mar = c(2, 2,1,1))
  qqexp(r)  
  par(mar = c(2, 2,1,1))
  drawHPPIntensity(object, events = events,
                   plot_events = TRUE,...)  
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.mmhp <- function(object, events, pzt, ...) {
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),
         heights = c(2, 2),widths = c(2, 2))
  r=compensator(object = object, events = events, pzt = pzt)  
  par(mar = c(2, 2,1,1))
  qqexp(r)  
  par(mar = c(2, 2,1,1))
  drawUniMMHPIntensity(mmhp = object, 
                       add=FALSE, ...)  
}


