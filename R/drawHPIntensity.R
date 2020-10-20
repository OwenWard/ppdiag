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
#' events <- sims$t
#' drawHPIntensity(hp_obj, start = 0, end = max(events), history = 0, events)
#' }
drawHPIntensity <- function(hp, 
                            start = 0, end = max(events), history=0, events,
                            color = 1, i = 1, add=FALSE, fit=FALSE, vec=rep(0.1,3), 
                            int_title="Hawkes Intensity") {
  n <- length(events)
  m <- length(history)
  lambda0 <- hp$lambda0
  alpha <- hp$alpha
  beta <- hp$beta
  old_events <- hp$events
  if(add==FALSE){
    #hawkes_par <- list(lambda0 = lambda0,alpha = alpha, beta = beta)
    #events <- c(history,t)
    #events <- t
	  
    if(is.null(old_events)){
      message("No events in object. Plotting provided events.")
    }
    else if(!is.null(old_events) && !all(events==old_events)){
        message("Events in object and events provided don't match. Plotting provided events.")
    }
    
    if(fit==TRUE){
      hp <- fithp(events=events,vec=vec)
      lambda0 <- hp$lambda0
      alpha <- hp$alpha
      beta <- hp$beta
    }
	  
    y_max <- hawkes_max_intensity(hp,events)
    ylim <-  c(0,y_max)
    graphics::plot(0, 0,
                   xlim = c(start, end),
                   ylim = ylim, type = "n", xlab = "Time", 
                   ylab = "Intensity",main = int_title)
    for(i in seq_along(events)){
	    graphics::points(x=events[i],y=0,pch=1,col="blue")
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
