#' Simulate a Markov Modulated Poisson Process
#'
#' Simulate Markov Modulated Poisson Process (including all the history) 
#' according to a mmpp object
#'
#' @param mmpp a mmpp object including its Q, delta, events, lambda0, c,
#' with lambda1 = lambda0(1 + c)
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
#' @return simulated Markov Modulated Poisson Process, 
#' including states of Markov Process, time of each 
#' transition of Markov Process, state at each event,
#'  times of Poisson events. Active state 2, inactive state 1
#' @noRd
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- pp_mmpp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, c = 1.2)
#' simulatemmpp(x, n = 10)
simulatemmpp <- function(mmpp, n = 1, start = 0, given_state = FALSE,
                         states = NULL, ...) {
  ## check c > 0
  # if(!is.null(mmhp$events)){
  #   stop("Event time already in the mmhp object.")
  # }
  m <- 2
  #------------------------
  if (sum(mmpp$delta) != 1) stop("Invalid delta")
  if (any(mmpp$delta == 1)) {
    initial <- (1:m)[as.logical(mmpp$delta)]
  } else {
    initial <- sample(m, 1, prob = mmpp$delta)
  }
  #------------------------
  Q <- mmpp$Q
  lambda0 <- mmpp$lambda0
  c <- mmpp$c
  lambda1 <- lambda0*(1 + c)
  
  old_events <- mmpp$events
  if(!is.null(old_events)){
    message("Events in the mmpp object will be overwritten by simulated events.")
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
        #   sim times of Poisson events in this state
        while (j < n + 2) {
          #   sim times of Poisson events
          ti <- t0 + rexp(1, rate = lambda1)
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
    # message(paste(n,"events simulated. To simulate up to endtime set given_states=TRUE and provide states."))
    mmpp$events <- events[1:(n + 1)]
    return(list(x = x[1:i], z = z[1:i], 
                events = mmpp$events, zt = zt[1:(n + 1)],
                start = x[1], end = x[i]))
  } else {
    x <- states$x
    z <- states$z
    ending <- states$ending
    zt[1] <- z[1]
    events[1] <- start
    lambda.max <- 0
    i <- 1 # index for state
    j <- 2 # index for event
    
    while (events[j - 1] <= ending & i < length(x)) {
      i <- i + 1
      t0 <- x[i - 1]
      
      if (z[i - 1] == 1) {
        while (events[j - 1] <= ending) {
          #   sim times of Poisson events
          ti <- t0 + rexp(1, rate = lambda1)
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
    mmpp$events <- events[1:(j - 1)][events[1:(j - 1)] <= ending]
    return(list(events = mmpp$events,
                zt = zt[1:(j - 1)][events[1:(j - 1)] <= ending]) )
  }
}
