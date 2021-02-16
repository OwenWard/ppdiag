#' Simulate a Markov Modulated Hawkes Process
#'
#' Simulate Markov Modulated Hawkes Process (including all the history) 
#' according to a mmhp object
#'
#' @param mmhp a mmhp object including its Q, delta, events, lambda0, 
#' lambda1, beta and alpha.
#' @param n number of points to simulate.
#' @param start start time for simulation
#' @param given_state if the hidden state trajectory is given.
#'  If `TRUE`, then simulate according to the given state. 
#'  Default to `FALSE`
#' @param states an object containing:
#'              - z: the states of Markov Process,
#'              - x: time of each transition of Markov process
#'              - ending: preset ending time for the process
#' @param ... other arguments.
#' @importFrom stats rexp
#'
#' @return simulated Markov Modulated Hawkes Process, 
#' including states of Markov Process, time of each 
#' transition of Markov Process, state at each event,
#'  times of events.
#' @noRd
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1,
#'  alpha = 0.8, beta = 1.2)
#' simulatemmhp(x, n = 10)
simulatemmhp <- function(mmhp, n = 1, start = 0, given_state = FALSE,
                         states = NULL, ...) {
  # if(!is.null(mmhp$events)){
  #   stop("Event time already in the mmhp object.")
  # }
  m <- 2
  #------------------------
  if (sum(mmhp$delta) != 1) stop("Invalid delta")
  if (any(mmhp$delta == 1)) {
    initial <- (1:m)[as.logical(mmhp$delta)]
  } else {
    initial <- sample(m, 1, prob = mmhp$delta)
  }
  #------------------------
  Q <- mmhp$Q
  lambda0 <- mmhp$lambda0
  lambda1 <- mmhp$lambda1
  alpha <- mmhp$alpha
  beta <- mmhp$beta
  
  old_events <- mmhp$events
  if(!is.null(old_events)){
    message("Events in the mmhp object will be overwritten by simulated events.")
  }
  
  if(alpha > beta) {
    stop("Require alpha less than beta for a stationary process")
  }
  if(is.null(Q)) {
    stop("No Q matrix specified")
  }

  Pi <- diag(m) - diag(1 / diag(Q)) %*% Q
  zt <- rep(NA, n + 1)
  events <- rep(NA, n + 1)
  #------------------------ initialization for Markov process
  #    the length of x and z may be too short
  #    gets extended later if required
  if (given_state == FALSE) {
    x <- rep(NA, n * 10)
    z <- rep(NA, n * 10)
    z[1] <- zt[1] <- initial
    x[1] <- events[1] <- start
    lambda.max <- 0
    i <- 1 # index for state
    j <- 2 # index for event
    #------------------------ initialization for Hawkes process

    while (j < n + 2) {
      i <- i + 1
      #   extend x and z if too short
      if (i > length(x)) {
        x <- c(x, rep(NA, n * 10))
        z <- c(z, rep(NA, n * 10))
      }
      #   sim time spent in Markov state y[i-1]
      z[i] <- sample(x = 1:m, size = 1, prob = Pi[(z[i - 1]), ])
      x[i] <- x[i - 1] + rexp(1, rate = -Q[z[i - 1], z[i - 1]])
      t0 <- x[i - 1]

      if (z[i - 1] == 1) {
        #   sim times of Hawkes Poisson events
        hp_obj <- list(lambda0 = lambda1,
                       alpha = alpha,
                       beta = beta)
        class(hp_obj) <- "hp"
        simulate.result <- suppressMessages( simulatehp(hp_obj, start = x[i - 1],
                                      end = x[i],
                                      history = events[1:(j - 1)]) )
        # while (is.null(simulate.result$events)){
        #   simulate.result <- simulatehp(hp_obj, start=x[i - 1], end=x[i],
        #                                 history=events[1:(j - 1)])
        # }
        hp <- simulate.result$events
        if( is.null(hp)) {
          hp <- 0
        }
        lambda.max <- ifelse(lambda.max > simulate.result$lambda.max,
                             lambda.max, simulate.result$lambda.max)
        if (!hp[1] == 0) {
          events[j:(j + length(hp) - 1)] <- hp
          zt[j:(j + length(hp) - 1)] <- z[i - 1]
          j <- j + length(hp)
          ## should this increment regardless?
        }
      }

      if (z[i - 1] == 2) {
        while (j < n + 2) {
          #   sim times of Poisson events
          ti <- t0 + rexp(1, rate = lambda0)
          if (ti < x[i]) {
            events[j] <- t0 <- ti
            zt[j] <- z[i - 1]
            j <- j + 1
          }
          else {
            break
          }
        }
      }
    }
    # x <- round(x,3)
    # events <- round(events,3)
    message(paste(n,"events simulated. To simulate up to endtime set given_states=TRUE and provide states."))
    mmhp$events <- events[1:(n + 1)]
    return(list(x = x[1:i], z = z[1:i], 
                events = mmhp$events, zt = zt[1:(n + 1)],
                lambda.max = lambda.max, start = x[1], end = x[i]))
  } else {
    x <- states$x
    z <- states$z
    ending <- states$ending
    zt[1] <- z[1]
    events[1] <- start
    lambda.max <- 0
    i <- 1 # index for state
    j <- 2 # index for event
    #------------------------ initialization for Hawkes process

    while (events[j - 1] <= ending & i < length(x)) {
      i <- i + 1
      t0 <- x[i - 1]

      if (z[i - 1] == 1) {
        #   sim times of Hawkes Poisson events
        hp_obj <- list(lambda0=lambda1,alpha=alpha,beta=beta)
        class(hp_obj) <- "hp"
        simulate.result <- suppressMessages( simulatehp(hp_obj, start=x[i - 1], end=x[i],
                                      history=events[1:(j - 1)]) )
        # while (is.null(simulate.result$events)){
        #   simulate.result <- simulatehp(hp_obj, start=x[i - 1], end=x[i],
        #                                 history=events[1:(j - 1)])
        # }
        hp <- simulate.result$events
        lambda.max <- ifelse(lambda.max > simulate.result$lambda.max,
                             lambda.max, simulate.result$lambda.max)
        if (!hp[1] == 0) {
          events[j:(j + length(hp) - 1)] <- hp
          zt[j:(j + length(hp) - 1)] <- z[i - 1]
          j <- j + length(hp)
        }
      }

      if (z[i - 1] == 2) {
        while (events[j - 1] <= ending) {
          #   sim times of Poisson events
          ti <- t0 + rexp(1, rate = lambda0)
          if (ti < x[i]) {
            events[j] <- t0 <- ti
            zt[j] <- z[i - 1]
            j <- j + 1
          }
          else {
            break
          }
        }
      }
    }
    message("Simulating up to endtime. To simulate desired length of events set given_states=FALSE and states=NULL.")
    mmhp$events <- events[1:(j - 1)][events[1:(j - 1)] <= ending]
    return(list(events = mmhp$events,
                zt = zt[1:(j - 1)][events[1:(j - 1)] <= ending])
                # lambda.max = lambda.max)
    )  
  }
}
