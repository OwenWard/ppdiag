#' Draw the intensity and q-q plot
#'
#' Draw the intensity and q-q plot for models
#'
#' @param object parameters for the models: hp, hpp, and mmhp
#' @param events event times (for mmhp, it's a list containing states of Markov Process, time of each 
#' transition of Markov Process, state at each event, and times of Poisson events)
#' @param ... further arguments
#' which is only important when using mmhp
#' @importFrom graphics par
#' @importFrom graphics layout
#' @export
intensityqqplot <- function(object, events, ...){
	UseMethod("intensityqqplot")
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.default <- function(object, events, ...) {
  cat("Please input the right model. Select from hp, hpp, and mmhp. ")   
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.hp <- function(object, events, ...) {
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),heights = c(2, 2),
         widths = c(2, 2))
  r <- compensator(object = object, events = events)  
  par(mar = c(2, 2,1,1))
  qqexp(r)
  par(mar = c(2, 2,1,1))
  drawHPIntensity(object, events = events, add = FALSE, ...)
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.hpp <- function(object, events, ...) {
	
  layout(mat = matrix(c(1, 2), nrow = 1, ncol = 2),
         heights = c(2, 2), widths = c(2, 2))
  r <- compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  qqexp(r)  
  par(mar = c(2, 2, 1, 1))
  drawHPPIntensity(object, events = events, int_title="Intensity plot of HPP",...)  
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.mmhp <- function(object, events, ...) {
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),
         heights = c(2, 2),widths = c(2, 2))
  r <- compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  qqexp(r)  
  par(mar = c(2, 2, 1, 1))
  drawUniMMHPIntensity(mmhp = object, events, add=FALSE, ...)  
}


