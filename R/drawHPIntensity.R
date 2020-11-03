#' Draw the intensity of Hawkes Process
#'
#' Draw the intensity of Hawkes Process, a helper function for
#'  'drawUniMMHPIntensity'
#' while also available independently
#'
#' @param hp object parameters for Hawkes process
#' @param start the start time of current state
#' @param end the end time of current state
#' @param history the past event times
#' @param events the event times happened in this state
#' @param color A specification for the default plotting color.
#' @param i state number this corresponds to a state jump directly before
#' which is only important when using mmhp
#' @param add whether to add the hawkes intensity to an existing plot
#' @param plot_events a boolean indicating whether events
#'  inputted will be plotted
#' @param vec vector of initial values of parameters used in fithp
#' @param int_title title of the intensity plot
#' @param fit a boolean indicating whether to fit a hp or 
#' use the passed object
#' @importFrom graphics curve
#' @importFrom graphics segments
#' @importFrom stats optimize
#' @importFrom graphics points
#' @export
#' @examples
#' \dontrun{
#' hp_obj <- hp(lambda0 = 0.1, alpha = 0.45, beta = 0.5)
#' sims <- simulatehp(hp_obj, start = 0, end = 20, history = 0)
#' events <- sims$events
#' drawHPIntensity(hp_obj, start = 0, end = max(events), history = 0, events)
#' }
drawHPIntensity <- function(hp, 
                            start = 0, end = max(events), history=0, events,
                            color = 1, i = 1, add=FALSE, fit=FALSE,
                            plot_events=FALSE, vec=NULL, 
                            int_title="Hawkes Intensity") {
  n <- length(events)
  m <- length(history)
  old_events <- hp$events
  
  if(add==FALSE){
    #hawkes_par <- list(lambda0 = lambda0,alpha = alpha, beta = beta)
    #events <- c(history,t)
    #events <- t
	  
    if(is.null(old_events)){
      if(fit==TRUE){
        message("Fitting provided events.")
        if(is.null(vec)){
          hp_obj <- fithp(events)
        }else{
          hp_obj <- fithp(vec=vec, events)
        }
        lambda0 <- hp$lambda0
        alpha <- hp$alpha
        beta <- hp$beta
      }else{
        message("Using the hp object. Set fit=TRUE to fit events. ")
        lambda0 <- hp$lambda0
        alpha <- hp$alpha
        beta <- hp$beta
      }
    }else if(!is.null(old_events) && !all(events==old_events)){
      if(fit==TRUE){
        message("Events in object and events provided don't match. Fitting provided events.")
        if(is.null(vec)){
          hp_obj <- fithp(events)
        }else{
          hp_obj <- fithp(vec=vec, events)
        }
        lambda0 <- hp$lambda0
        alpha <- hp$alpha
        beta <- hp$beta
      }else{
        message("Events in object and events provided don't match. Using the object. ")
        lambda0 <- hp$lambda0
        alpha <- hp$alpha
        beta <- hp$beta
        events <- old_events
        end <- max(events)
        n <- length(events)
      }
    }else{
      lambda0 <- hp$lambda0
      alpha <- hp$alpha
      beta <- hp$beta
      n <- length(events)
    }
    
    y_max <- hawkes_max_intensity(hp,events)
    ylim <-  c(0,y_max)
    graphics::plot(0, 0,
                   xlim = c(start, end),
                   ylim = ylim, type = "n", xlab = "Time", 
                   ylab = "Intensity",main = int_title)
    if(plot_events==TRUE){
      for(j in seq_along(events)){
        graphics::points(x=events[j],y=0,pch=1,col="blue")
      }
    }
    
    if (n == 0) {
      if (i == 1) {
        segments(x0 = start, x1 = end, y0 = lambda0)
      } else {
        lambda.n <- function(s) lambda0 + 
          alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        graphics::segments(x0 = start, y0 = lambda0, y1 = lambda.n(end),
                           lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start,
                        to = end, add = TRUE, col = color)
      }
    } else {
      if (i == 1) {
        graphics::segments(x0 = start, x1 = events[1], 
                           y0 = lambda0, col = color)
        segments(x0 = events[1], y0 = lambda0, 
                 y1 = lambda0 + alpha, col = color)
      } else {
        lambda.n <- function(s) lambda0 + 
          alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        segments(x0 = start, y0 = lambda0, y1 = lambda.n(start),
                 lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start, 
                        to = events[1], add = TRUE, col = color)
        segments(x0 = events[1], y0 = lambda.n(events[1]),
                 y1 = lambda.n(events[1]) + alpha, col = color)
      }
      if (n > 1) {
        for (j in 1:(n - 1)) {
          lambda.n <- function(s) lambda0 + 
            alpha * sum(exp(-beta * (rep(s, m + j) - c(history, events[1:j]))))
          new.lambda.n <- Vectorize(lambda.n)
          curve(new.lambda.n, from = events[j], to = events[j + 1],
                add = TRUE, col = color)
          segments(x0 = events[j + 1], y0 = lambda.n(events[j + 1]),
                   y1 = lambda.n(events[j + 1]) + alpha, col = color)
        }
      }
      lambda.n <- function(s) lambda0 + 
        alpha * sum(exp(-beta * (rep(s, m + n) - c(history, events[1:n]))))
      new.lambda.n <- Vectorize(lambda.n)
      curve(new.lambda.n, from = events[n], to = end, add = TRUE, col = color)
      segments(x0 = end, y0 = lambda.n(end), y1 = lambda0, lty = 2, col = color)
      # if (t[n]==end){
      #   max=c(max,new.lambda.n(end))
      # }
      # else{
      #   max=c(max,optimize(new.lambda.n, interval=c(t[n], end),
      # maximum=TRUE)$objective)
      # }
    }
  }
  else {
    # to add to an already created plot
    lambda0 <- hp$lambda0
    alpha <- hp$alpha
    beta <- hp$beta
    
    if (n == 0) {
      if (i == 1) {
        segments(x0 = start, x1 = end, y0 = lambda0)
      } else {
        lambda.n <- function(s) lambda0 + 
          alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        graphics::segments(x0 = start, y0 = lambda0, 
                           y1 = lambda.n(end), lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start,
                        to = end, add = TRUE, col = color)
      }
    } else {
      if (i == 1) {
        graphics::segments(x0 = start, x1 = events[1],
                           y0 = lambda0, col = color)
        segments(x0 = events[1], y0 = lambda0,
                 y1 = lambda0 + alpha, col = color)
      } else {
        lambda.n <- function(s) lambda0 + 
          alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        segments(x0 = start, y0 = lambda0, y1 = lambda.n(start),
                 lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start, to = events[1],
                        add = TRUE, col = color)
        segments(x0 = events[1], y0 = lambda.n(events[1]),
                 y1 = lambda.n(events[1]) + alpha, col = color)
      }
      if (n > 1) {
        for (j in 1:(n - 1)) {
          lambda.n <- function(s) lambda0 + 
            alpha * sum(exp(-beta * (rep(s, m + j) - c(history, events[1:j]))))
          new.lambda.n <- Vectorize(lambda.n)
          curve(new.lambda.n, from = events[j], to = events[j + 1],
                add = TRUE, col = color)
          segments(x0 = events[j + 1], y0 = lambda.n(events[j + 1]),
                   y1 = lambda.n(events[j + 1]) + alpha, col = color)
        }
      }
      lambda.n <- function(s) lambda0 + 
        alpha * sum(exp(-beta * (rep(s, m + n) - c(history, events[1:n]))))
      new.lambda.n <- Vectorize(lambda.n)
      curve(new.lambda.n, from = events[n], to = end, add = TRUE, col = color)
      segments(x0 = end, y0 = lambda.n(end), y1 = lambda0, 
               lty = 2, col = color)
    }
  }
  legend("topleft", "Events", col = "blue", pch = 1)
}
