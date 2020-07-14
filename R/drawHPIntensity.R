#' Draw the intensity of Hawkes Process
#'
#' Draw the intensity of Hawkes Process, a helper function for 'drawUniMMHPIntensity'
#' while also available independently
#'
#' @param object parameters for Hawkes process
#' @param start the start time of current state
#' @param end the end time of current state
#' @param history the past event times
#' @param t the event times happened in this state
#' @param color A specification for the default plotting color.
#' @param i state number this corresponds to a state jump directly before
#' which is only important when using mmhp
#' @param add whether to add the hawkes intensity to an existing plot
#' @importFrom graphics curve
#' @importFrom graphics segments
#' @importFrom stats optimize
#' @importFrom graphics points


#' @export
#' @examples
#' hp_obj <- hp(lambda0 = 0.1,alpha = 0.45,beta = 0.5)
#' sims <- simulatehp(hp_obj,start = 0, end = 100, history = 0)
#' events=sims$t
#' drawHPIntensity(hp_obj, start=0, end=max(events), history=0, t=events)

drawHPIntensity <- function(object, 
                            start, end, history=0, t,
                            color = 1, i = 1, add=FALSE) {
  n <- length(t)
  m <- length(history)
  lambda0 = object$lambda0
  alpha = object$alpha
  beta = object$beta
  if(add==FALSE){
    #hawkes_par <- list(lambda0 = lambda0,alpha = alpha, beta = beta)
    #events <- c(history,t)
    events <- t
    y_max <- hawkes_max_intensity(object,events)
    ylim = c(0,y_max)
    graphics::plot(0, 0,
                   xlim = c(start, end),
                   ylim = ylim, type = "n", xlab = "Time", 
                   ylab = "Intensity",main = 'Hawkes')
    for(i in 1:length(events)){
	    graphics::points(x=events[i],y=0,pch=1,col="blue")
	  }
    
    if (n == 0) {
      if (i == 1) {
        segments(x0 = start, x1 = end, y0 = lambda0)
      } else {
        lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        graphics::segments(x0 = start, y0 = lambda0, y1 = lambda.n(end), lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start, to = end, add = TRUE, col = color)
      }
    } else {
      if (i == 1) {
        graphics::segments(x0 = start, x1 = t[1], y0 = lambda0, col = color)
        segments(x0 = t[1], y0 = lambda0, y1 = lambda0 + alpha, col = color)
      } else {
        lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        segments(x0 = start, y0 = lambda0, y1 = lambda.n(start), lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start, to = t[1], add = TRUE, col = color)
        segments(x0 = t[1], y0 = lambda.n(t[1]), y1 = lambda.n(t[1]) + alpha, col = color)
      }
      if (n > 1) {
        for (j in 1:(n - 1)) {
          lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m + j) - c(history, t[1:j]))))
          new.lambda.n <- Vectorize(lambda.n)
          curve(new.lambda.n, from = t[j], to = t[j + 1], add = TRUE, col = color)
          segments(x0 = t[j + 1], y0 = lambda.n(t[j + 1]), y1 = lambda.n(t[j + 1]) + alpha, col = color)
        }
      }
      lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m + n) - c(history, t[1:n]))))
      new.lambda.n <- Vectorize(lambda.n)
      curve(new.lambda.n, from = t[n], to = end, add = TRUE, col = color)
      segments(x0 = end, y0 = lambda.n(end), y1 = lambda0, lty = 2, col = color)
      # if (t[n]==end){
      #   max=c(max,new.lambda.n(end))
      # }
      # else{
      #   max=c(max,optimize(new.lambda.n, interval=c(t[n], end), maximum=TRUE)$objective)
      # }
    }
  }
  else {
    # to add to an already created plot
    if (n == 0) {
      if (i == 1) {
        segments(x0 = start, x1 = end, y0 = lambda0)
      } else {
        lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        graphics::segments(x0 = start, y0 = lambda0, y1 = lambda.n(end), lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start, to = end, add = TRUE, col = color)
      }
    } else {
      if (i == 1) {
        graphics::segments(x0 = start, x1 = t[1], y0 = lambda0, col = color)
        segments(x0 = t[1], y0 = lambda0, y1 = lambda0 + alpha, col = color)
      } else {
        lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        segments(x0 = start, y0 = lambda0, y1 = lambda.n(start), lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start, to = t[1], add = TRUE, col = color)
        segments(x0 = t[1], y0 = lambda.n(t[1]), y1 = lambda.n(t[1]) + alpha, col = color)
      }
      if (n > 1) {
        for (j in 1:(n - 1)) {
          lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m + j) - c(history, t[1:j]))))
          new.lambda.n <- Vectorize(lambda.n)
          curve(new.lambda.n, from = t[j], to = t[j + 1], add = TRUE, col = color)
          segments(x0 = t[j + 1], y0 = lambda.n(t[j + 1]), y1 = lambda.n(t[j + 1]) + alpha, col = color)
        }
      }
      lambda.n <- function(s) lambda0 + alpha * sum(exp(-beta * (rep(s, m + n) - c(history, t[1:n]))))
      new.lambda.n <- Vectorize(lambda.n)
      curve(new.lambda.n, from = t[n], to = end, add = TRUE, col = color)
      segments(x0 = end, y0 = lambda.n(end), y1 = lambda0, lty = 2, col = color)
    }
  }

}
