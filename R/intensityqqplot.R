#' Draw Intensity of Fitted Point Process and QQ-Plot of Rescaled Events
#' 
#'
#' Draw the intensity and q-q plot for models
#'
#' @param object parameters for the models: hp, hpp, and mmhp
#' @param events event times
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
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2), heights = c(2, 2),
         widths = c(2, 2))
  r <- compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)
  par(mar = c(2, 2, 1, 1))
  drawHPIntensity(object, start = 0, end = max(events),
                  events = events, add = FALSE, plot_events = TRUE, ...)
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.hpp <- function(object, events, ...) {
	
  layout(mat = matrix(c(1, 2), nrow = 1, ncol = 2),
         heights = c(2, 2), widths = c(2, 2))
  r <- compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)  
  par(mar = c(2, 2, 1, 1))
  drawHPPIntensity(object, events = events,
                   plot_events = TRUE, 
                   int_title = "Intensity plot of HPP", ...)  
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.mmhp <- function(object, events, ...) {
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),
         heights = c(2, 2),widths = c(2, 2))
  r <- compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)  
  par(mar = c(2, 2, 1, 1))
  drawUniMMHPIntensity(mmhp = object, 
                       add=FALSE, ...)  
}


