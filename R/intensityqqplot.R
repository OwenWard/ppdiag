#' Draw intensity of fitted point process and QQ-Plot of rescaled events
#' 
#'
#' Draw the intensity and q-q plot for models
#'
#' @param object parameters for the models: hp, hpp, and mmhp
#' @param events event times
#' @param markov_states only for mmhp and mmpp, markov states simulation output 
#' @importFrom graphics par
#' @importFrom graphics layout
#' @return no return value, intensity and qq-plot in a single plot
#' @export
intensityqqplot <- function(object, events, markov_states){
	UseMethod("intensityqqplot")
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.default <- function(object, events, markov_states) {
  cat("Please input the right model. Select from hp, hpp, and mmhp. ")   
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.hp <- function(object, events, markov_states = NULL) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2), heights = c(2, 2),
         widths = c(2, 2))
  r <- pp_compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)
  par(mar = c(2, 2, 1, 1))
  drawHPIntensity(object, start = 0, end = max(events),
                  events = events, add = FALSE, plot_events = TRUE)
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.hpp <- function(object, events, markov_states = NULL) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  layout(mat = matrix(c(1, 2), nrow = 1, ncol = 2),
         heights = c(2, 2), widths = c(2, 2))
  r <- pp_compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)  
  par(mar = c(2, 2, 1, 1))
  drawHPPIntensity(object, events = events,
                   plot_events = TRUE, 
                   int_title = "Intensity plot of HPP")  
}


#' @rdname intensityqqplot
#' @export
intensityqqplot.mmpp <- function(object, events = markov_states$events, markov_states) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),
         heights = c(2, 2),widths = c(2, 2))
  r <- pp_compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)  
  par(mar = c(2, 2, 1, 1))
  drawUniMMPPIntensity(mmpp = object, simulation = markov_states,
                       add=FALSE)  
}

#' @rdname intensityqqplot
#' @export
intensityqqplot.mmhp <- function(object, events = markov_states$events, markov_states) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2),
         heights = c(2, 2),widths = c(2, 2))
  r <- pp_compensator(object = object, events = events)  
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)  
  par(mar = c(2, 2, 1, 1))
  drawUniMMHPIntensity(mmhp = object, simulation = markov_states,
                       add=FALSE)  
}


