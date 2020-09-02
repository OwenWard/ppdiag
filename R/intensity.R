#' Compute the intensity of social network model
#'
#' Take an object of MMHP/HP/MMPP and generate its intensity function accordingly
#'
#' @param object an object of MMHP/HP/MMPP
#' For example, MMHP object should includ its state, state_time, events, lambda0, lambda1, beta and alpha.
#' @param event the observed/simulated events
#' @param method the method used to calculate intensity.
#'   The candidates are: `numeric`, and `atevent`, default to `numeric`. 
#'   ### function currently removed as not needed
#' @return The intensity function of MMHP
#' @export
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1, alpha = 0.8, beta = 1.2)
#' y <- simulatemmhp(x)
#'## z <- intensity(x, y) ### need to fix this, due to time.vec being missing
intensity <- function(object, event, method = "numeric") {
  UseMethod("intensity")
}

#' @rdname intensity
#' @export
intensity.default <- function(object, event, method = "numeric") {
  cat("please input the right model")
}

#' @rdname intensity
#' @export
intensity.mmhp <- function(object, event, method = "numeric") {
  events <- event$events
  lambda0 <- object$lambda0
  lambda1 <- object$lambda1
  alpha <- object$alpha
  beta <- object$beta
  n <- length(events)
  # if (method == "function") {
  #   # return a function of intensity
  #   state <- event$z
  #   state_time <- event$x
  #   m <- length(state)
  #   intensity <- function(x) {
  #     y <- 0
  #     for (i in 1:(m - 1)) {
  #       if (state[i] == 1) {
  #         hawkes_time <- events[events >= state_time[i] & events < state_time[i + 1]]
  #         if (i == 1) hawkes_time <- hawkes_time[-1]
  #         history <- events[events < state_time[i]]
  #         hp_object <- hp(lambda0 = lambda1, alpha, beta)
  #         hp_event <- list(start = state_time[i], end = state_time[i + 1],
  #                          history = history[-1], hawkes_time = hawkes_time)
  #         HPfunc <- intensity.hp(object = hp_object, event = hp_event)
  #         if (x >= state_time[i] & x <= state_time[i + 1]) {
  #           y <- HPfunc(x)
  #         }
  #       } else {
  #         if (x >= state_time[i] & x <= state_time[i + 1]) {
  #           y <- lambda0
  #         }
  #       }
  #     }
  #     return(y)
  #   }
  #   return(Vectorize(intensity))
  # } 
  if (method == "numeric") {
    # return the numeric intensity value at each time segment
    time.vec <- event$time_segment
    ## this is empty here by default
    
    latent.vec <- event$latent_mean
    hp_object <- hp(lambda1, alpha, beta)
    hp_event <- list(events = events, time.vec = time.vec)
    lambda1.t <- intensity.hp(hp_object, hp_event, method = "numeric")
    lambda.t <- lambda1.t * latent.vec + lambda0 * (1 - latent.vec)
    return(lambda.t)
  } 
  else if (method == "atevent") {
    # return the intensity evaluates at event times (output is an vector)
    latent_z <- event$z
    if (events[1] == 0) {
      events <- events[-1]
    }
    if (length(latent_z) == (length(events) + 1)) {
      latent_z <- latent_z[-1]
    }
    lambda.t <- rep(lambda0, length(events))
    r <- 0
    for (i in c(1:length(events))) {
      if (i > 1) {
        r <- exp(-beta * (events[i] - events[i - 1])) * (1 + r)
      }
      if (latent_z[i] == 1) {
        lambda.t[i] <- lambda1 + alpha * r
      }
    }
    return(lambda.t)
  } # else if (method =="attime"){
  # return intensity evaluates at event times (output is an vector)
  #   events<-event$events
  #   latent_z <-event$z
  #   latent$x <-event$x
  #   current_time <- event$current_time
  # }
}

#' @rdname intensity
#' @export
intensity.hp <- function(object, event, method = "numeric") {
  # if (method == "function") {
  #   lambda <- object$lambda0
  #   alpha <- object$alpha
  #   beta <- object$beta
  #   start <- event$start
  #   end <- event$end
  #   history <- event$history
  #   hawkes_time <- event$hawkes_time
  #   n <- length(hawkes_time)
  #   m <- length(history)
  #   intensity <- function(x) {
  #     y <- 0
  #     if (n == 0) {
  #       if (i == 1) {
  #         if (x >= start & x <= end) {
  #           y <- lambda
  #         }
  #       } else {
  #         lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m) - history)))
  #         new.lambda.n <- Vectorize(lambda.n)
  #         if (x >= start & x <= end) {
  #           y <- new.lambda.n(x)
  #         }
  #       }
  #     } else {
  #       if (i == 1) {
  #         if (x >= start & x < hawkes_time[1]) {
  #           y <- lambda
  #         }
  #       } else {
  #         lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m) - history)))
  #         new.lambda.n <- Vectorize(lambda.n)
  #         if (x >= start & x < hawkes_time[1]) {
  #           y <- new.lambda.n(x)
  #         }
  #       }
  #       if (n > 1) {
  #         for (j in 1:(n - 1)) {
  #           lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m + j) - c(history, hawkes_time[1:j]))))
  #           new.lambda.n <- Vectorize(lambda.n)
  #           if (x >= hawkes_time[j] & x < hawkes_time[j + 1]) {
  #             y <- new.lambda.n(x)
  #           }
  #         }
  #       }
  #       lambda.n <- function(s) lambda + alpha * sum(exp(-beta * (rep(s, m + n) - c(history, hawkes_time[1:n]))))
  #       new.lambda.n <- Vectorize(lambda.n)
  #       if (x >= hawkes_time[n] & x <= end) {
  #         y <- new.lambda.n(x)
  #       }
  #     }
  #     return(y)
  #   }
  #   return(Vectorize(intensity))
  # } 
  if (method == "numeric") {
    time.vec <- event$time.vec
    events <- event$events
    lambda<-object$lambda0
    beta<-object$beta
    alpha<-object$alpha
    lambda1.t <- rep(0, length(time.vec))
    event.idx <- 1


    r <- 0
    for (i in c(1:length(time.vec))) {
      current.t <- time.vec[i]
      if (event.idx < length(events)) {
        if (current.t > events[event.idx + 1]) {
          event.idx <- event.idx + 1
          r <- exp(-beta * (events[event.idx] - events[event.idx - 1])) * (1 + r)
        }
      }

      if (current.t <= events[1]) {
        lambda1.t[i] <- lambda
      } else {
        lambda1.t[i] <- lambda + alpha * exp(-beta * (current.t - events[event.idx])) * (1 + r)
      }
    }

    return(lambda1.t)
  } 
  else if (method == "integral") {
    # This function is used to compute \int_0^T \lambda(u) du
    # input object: parameters for Hawkes process, include lambda0, alpha, beta
    #       events: vector of event happening time
    #       T: termination time
    # output result: \int_0^T \lambda(u) du

    lambda0 <- object$lambda0
    alpha <- object$alpha
    beta <- object$beta
    events <- event$events
    termination <- event$termination
    N <- length(events)
    r <- 0

    if (N > 1) {
      for (i in 2:N) {
        r <- exp(-beta * (events[i] - events[i - 1])) * (r + 1)
      }


      if (N == 0) {
        result <- lambda0 * termination
      } else {
        result <- lambda0 * termination + 
          alpha / beta * (N - (1 + r) * exp(-beta * (termination - events[N])))
      }

      return(result)
    }
  }
}

#' @rdname intensity
#' @export
intensity.mmpp <- function(object, event, method = "numeric") {
  ## latent.vec is vector with same length as time.vec, 
  ## each entry is the probability at state 1
  lambda0 <- object$lambda0
  c <- object$c
  latent.vec <- event$latent.vec
  lambda.t <- lambda0 * (1 + c) * latent.vec + lambda0 * (1 - latent.vec)
  return(lambda.t)
}

#' @rdname intensity
#' @export
intensity.hpp <- function(object, event, method = "numeric"){
  lambda=object$lambda
  start=object$start
  end=object$end
  n=object$n
  if(is.null(n)){
    n=rpois(n=1,lambda=lambda*end)
  }
  # if(method=="function"){
  #   intensity <- function(x) {
  #     l=lambda
  #     return(l)
  #   }
  #   return(Vectorize(intensity))
  # } 
  if (method == "numeric"){
    return(rep(lambda,n))
  } else if (method == "integral") {
    return ((end-start)*lambda)
  }  
}
