#' Compute the intensity of a temporal point process
#'
#' Take a point process object and compute the intensity
#' function accordingly, either numerically or at each event.
#' For processes with a latent state this will be inferred.
#'
#' @param object an object of MMHP/HP/HPP/MMPP
#' @param event_info list object, containing events, start and end of
#' observation period
#' @param method the method used to calculate intensity.
#'   The candidates are: `numeric`, `atevent`, and `integral`,
#'   default to `numeric`.
#' @param steps the number of pieces the observation window will be
#' broken into for numeric integration, default 1000
#' @return The intensity function of the specified point process, either
#' at the given events, integrated over the observation time, or numerically
#' @noRd
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- pp_mmhp(Q,
#'   delta = c(1 / 3, 2 / 3), lambda0 = 0.9,
#'   lambda1 = 1.1, alpha = 0.8, beta = 1.2
#' )
#' y <- pp_simulate(x, n = 10)
#' z <- intensity.mmhp(x, y)
pp_intensity <- function(object, event_info,
                         method = "numeric", steps = 1000) {
  UseMethod("pp_intensity")
}


pp_intensity.default <- function(object, event_info,
                                 method = "numeric", steps = 1000) {
  cat("please input the right model")
}


pp_intensity.mmhp <- function(object,
                              event_info,
                              method = "numeric",
                              steps = 1000) {
  events <- event_info$events
  start <- event_info$start
  end <- event_info$end
  lambda0 <- object$lambda0
  lambda1 <- object$lambda1
  alpha <- object$alpha
  beta <- object$beta
  n <- length(events)
  event_state <- mmhp_event_state(
    params = object,
    events = events, start = start
  )
  if (method == "numeric") {
    # return the numeric intensity value at each time segment
    time.vec <- seq(from = start, to = end, length.out = steps)

    latent_inter <- interpolate_mmhp_latent(
      params = object,
      events = events,
      zt = event_state$zt
    )
    # then use a step function on this
    step_fun_est <- stepfun(latent_inter$x.hat, 2 - latent_inter$z.hat)
    latent.vec <- step_fun_est(time.vec)
    ###
    hp_object <- pp_hp(lambda1, alpha, beta)
    hp_event <- list(events = events, start = start, end = end)
    lambda1.t <- pp_intensity.hp(hp_object, hp_event, method = "numeric")
    lambda.t <- lambda1.t * latent.vec + lambda0 * (1 - latent.vec)
    return(lambda.t)
  }
  if (method == "atevent") {
    # return the intensity evaluates at event times (output is an vector)
    ## compute latent state of events here

    latent_z <- event_state$zt
    if (events[1] == 0) {
      events <- events[-1]
    }
    if (length(latent_z) == (length(events) + 1)) {
      latent_z <- latent_z[-1]
    }
    lambda.t <- rep(lambda0, length(events))
    r <- 0
    for (i in seq_along(events)) {
      if (i > 1) {
        r <- exp(-beta * (events[i] - events[i - 1])) * (1 + r)
      }
      if (latent_z[i] == 1) {
        lambda.t[i] <- lambda1 + alpha * r
      }
    }
    return(lambda.t)
  }
  else if (method == "integral") {
    time.vec <- seq(from = start, to = end, length.out = steps)

    latent_inter <- interpolate_mmhp_latent(
      params = object,
      events = events,
      zt = event_state$zt
    )
    # then use a step function on this
    step_fun_est <- stepfun(latent_inter$x.hat, 2 - latent_inter$z.hat)
    latent.vec <- step_fun_est(time.vec)
    ###
    hp_object <- pp_hp(lambda1, alpha, beta)
    hp_event <- list(events = events, time.vec = time.vec)
    lambda1.t <- pp_intensity.hp(hp_object, hp_event, method = "numeric")
    lambda.t <- lambda1.t * latent.vec + lambda0 * (1 - latent.vec)
    return(sum(lambda1.t) * (time.vec[2] - time.vec[1]))
  }
}


pp_intensity.hp <- function(object,
                            event_info,
                            method = "numeric",
                            steps = 1000) {
  if (method == "numeric") {
    start <- event_info$start
    end <- event_info$end
    time.vec <- seq(from = start, to = end, length.out = steps)
    events <- event_info$events
    lambda <- object$lambda0
    beta <- object$beta
    alpha <- object$alpha
    lambda1.t <- rep(0, length(time.vec))
    event.idx <- 1
    r <- 0
    for (i in seq_along(time.vec)) {
      current.t <- time.vec[i]
      if (event.idx < length(events)) {
        if (current.t > events[event.idx + 1]) {
          event.idx <- event.idx + 1
          r <- exp(-beta * (events[event.idx] -
            events[event.idx - 1])) * (1 + r)
        }
      }

      if (current.t <= events[1]) {
        lambda1.t[i] <- lambda
      } else {
        lambda1.t[i] <- lambda +
          alpha * exp(-beta * (current.t - events[event.idx])) * (1 + r)
      }
    }

    return(lambda1.t)
  }
  else if (method == "integral") {
    # This function is used to compute \int_0^T \lambda(u) du
    # input object: parameters for Hawkes process, include lambda0,
    # alpha, beta
    #       events: vector of event happening time
    #       T: termination time
    # output result: \int_0^T \lambda(u) du

    lambda0 <- object$lambda0
    alpha <- object$alpha
    beta <- object$beta
    events <- event_info$events
    start <- event_info$start
    end <- event_info$end
    N <- length(events)
    r <- 0

    if (N > 1) {
      for (i in 2:N) {
        r <- exp(-beta * (events[i] - events[i - 1])) * (r + 1)
      }
      if (N == 0) {
        result <- lambda0 * (end - start)
      } else {
        result <- lambda0 * (end - start) +
          alpha / beta * (N - (1 + r) *
            exp(-beta * (end - events[N])))
      }

      return(result)
    }
  }
}


pp_intensity.mmpp <- function(object, event_info,
                              method = "numeric",
                              steps = 1000) {
  ## latent.vec is vector with same length as time.vec,
  ## each entry is the probability at state 1
  lambda0 <- object$lambda0
  c <- object$c
  events <- event_info$events
  start <- event_info$start
  end <- event_info$end
  time.vec <- seq(from = start, to = end, length.out = steps)
  event_state <- mmpp_event_state(
    params = object, events = events,
    start = start
  )
  if (method == "numeric") {
    latent_inter <- mmpp_latent(
      params = object,
      events = events,
      zt = event_state$zt
    )
    step_fun_est <- stepfun(latent_inter$x.hat, 2 - latent_inter$z.hat)
    latent.vec <- step_fun_est(time.vec)
    lambda.t <- lambda0 * (1 + c) * latent.vec + lambda0 * (1 - latent.vec)
    return(lambda.t)
  }
  if (method == "atevent") {
    lam_vec <- c(lambda0, lambda0 * (1 + c))
    return(lam_vec[c(event_state$zt)])
  }
  if (method == "integral") {
    latent_inter <- mmpp_latent(
      params = object,
      events = events,
      zt = event_state$zt
    )
    ## then use end to sum these over the length.
    ## this could probably be cleaner
    jumps <- latent_inter$x.hat
    states <- latent_inter$z.hat
    jumps <- jumps[jumps < end]
    states <- states[jumps < end]
    return(diff(jumps) * states[-length(states)] +
      (end - jumps[length(jumps)]) * states[length(states)])
  }
}


pp_intensity.hpp <- function(object, event_info,
                             method = "numeric") {
  lambda <- object$lambda
  start <- event_info$start
  end <- event_info$end
  events <- event_info$events

  n <- length(events)

  if (method == "atevent") {
    return(rep(lambda, n))
  }
  else if (method == "integral") {
    return((end - start) * lambda)
  }
  else if (method == "numeric") {
    return(rep(lambda, n))
  }
}
