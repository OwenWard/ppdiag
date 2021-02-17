#' Draw the intensity of Hawkes Process
#'
#' Draw the intensity of a Hawkes Process
#'
#' @param hp object parameters for Hawkes process. 
#' @param events the event times happened in this state
#' @param int_title title of the intensity plot
#' @param start the start time of current state
#' @param end the end time of current state
#' @param history the past event times
#' @param color specify the default plotting color.
#' @param i state number, used only for drawUniMMHPIntensity
#' @param add whether to add the hawkes intensity to an existing plot, used
#' for drawUniMMHPIntensity
#' @param plot_events indicate whether events will be plotted
#' @param fit a boolean indicating whether to fit a new HP to events
#' @importFrom graphics curve
#' @importFrom graphics segments
#' @importFrom graphics points
#' @export
#' @examples
#' \dontrun{
#' hp_obj <- pp_hp(lambda0 = 0.5, alpha = 0.45, beta = 0.5)
#' events <- pp_simulate(hp_obj, start = 0, end = 20)
#' drawHPIntensity(hp_obj,events)
#' }
drawHPIntensity <- function(hp = NULL , events,
                            int_title = "Hawkes Intensity",
                            start = 0,
                            end = max(events),
                            history = NULL,
                            color = 1,
                            i = 1,
                            add = FALSE,
                            fit = FALSE,
                            plot_events = TRUE) {
  n <- length(events)
  m <- length(history)
  old_events <- hp$events
  
  if(add==FALSE){
    #hawkes_par <- list(lambda0 = lambda0, alpha = alpha, beta = beta)
    #events <- c(history,t)
    #events <- t
	  
    if(is.null(old_events)){
      if(is.null(events)){
        stop("Events must be provided either in the object or in the events argument. ")
      }
      if(fit==TRUE){
        message("Fitting provided events.")
        hp <- fithp(events)
        lambda0 <- hp$lambda0
        alpha <- hp$alpha
        beta <- hp$beta
      }else{
        message("Using the hp object. Set fit = TRUE to fit events provided.")
        lambda0 <- hp$lambda0
        alpha <- hp$alpha
        beta <- hp$beta
      }
    }else{
      if(is.null(events)){
        message("No events provided. Using the hp object.")
        lambda0 <- hp$lambda0
        alpha <- hp$alpha
        beta <- hp$beta
        events <- hp$events
        if(start>0){
          start <- min(events)
        }
        end <- max(events)
      }else{
        if(fit==TRUE){
          message("Fitting provided events. Set events=NULL to use the events in object.")
          hp_obj <- fithp(events)
          lambda0 <- hp$lambda0
          alpha <- hp$alpha
          beta <- hp$beta
        }else{
          message("Using the hp object. Set fit=TRUE to fit events provided. ")
          lambda0 <- hp$lambda0
          alpha <- hp$alpha
          beta <- hp$beta
        }
      }
    }
    
    
    y_max <- hawkes_max_intensity(hp,events)
    ylim <-  c(0,y_max + 2)
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
        segments(x0 = events[1],
                 y0 = lambda0,
                 y1 = lambda0 + alpha,
                 col = color)
      } else {
        lambda.n <- function(s) lambda0 + 
          alpha * sum(exp(-beta * (rep(s, m) - history)))
        new.lambda.n <- Vectorize(lambda.n)
        segments(x0 = start, y0 = lambda0, y1 = lambda.n(start),
                 lty = 2, col = color)
        graphics::curve(new.lambda.n, from = start, 
                        to = events[1], add = TRUE, col = color)
        segments(x0 = events[1], 
                 y0 = lambda.n(events[1]),
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
    legend("topleft", c("Events", "Intensity"),
           col = c("blue", "black"), 
           pch = c(1,NA),
           lty = c(NA, 1), cex = 0.75)
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
  
}
