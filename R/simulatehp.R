#' Simulate Hawkes process events
#'
#' Simulate Hawkes process during active state (including all the history),
#'  a helper function for "simulationmmhp" and also available
#'  independently
#'
#' @param hp hawkes process object, including parameters in list type
#' (lambda0, alpha, beta, events)
#' @param history the past event times
#' @param start start time of the Hawkes process
#' @param end end time of the Hawkes process
#' @param n number of events
#' @param verbose whether to output informative messages as running
#' @importFrom stats runif
#' @return simulated Hawkes Process
#' @noRd
#' @examples
#' hp_obj <- pp_hp(lambda0 = 0.1, alpha = 0.45, beta = 0.5)
#' simulatehp(hp_obj, start = 0, end = 100)
simulatehp <- function(hp, start = 0,
                       end = NULL, history = NULL,
                       n = NULL, verbose = FALSE) {
  old_events <- hp$events
  if (!is.null(old_events)) {
    if (verbose == TRUE) {
      message("Events in the hp object will be overwritten by simulated events.")
    }
  }
  lambda0 <- hp$lambda0
  alpha <- hp$alpha
  beta <- hp$beta
  if (alpha >= beta) {
    stop("Stationary hawkes process requires alpha<beta.")
  }
  if (!is.null(start) && !is.null(end)) {
    if (start >= end) {
      return(NULL)
    }
  }
  if (is.null(history)) {
    j0 <- 2
  }
  else {
    j0 <- length(history) + 1
  }
  lambda.star <- ifelse(j0 == 2, lambda0, lambda0 +
    alpha * sum(exp(-beta * (rep(start, j0 - 2) -
      history[2:(j0 - 1)]))))
  lambda.max <- lambda.star
  t <- c()
  i <- 1
  U <- runif(1)
  while (U == 0) {
    U <- runif(1)
  }
  s <- -log(U) / lambda.star
  ti <- start + s

  if (!is.null(n)) {
    if (n <= 0) {
      stop("n must be positive for simulation.")
    }
    if (!is.null(end)) {
      if (verbose == TRUE) {
        message(paste(n, "events simulated. To simulate up to endtime set n=NULL."))
      }
    }
    repeat {
      if (i > n) {
        break
      }

      lambda.star <- lambda.star + alpha
      t <- append(t, ti)


      repeat{
        U <- runif(1)
        while (U == 0) {
          U <- runif(1)
        }
        s <- s - log(U) / lambda.star
        ti <- start + s
        lambda.s <- lambda0 +
          alpha * sum(exp(-beta * c(rep(ti, i) -
            t[1:i], rep(ti, j0 - 1) -
            history[1:j0 - 1])))
        D <- runif(1)
        while (D == 0) {
          D <- runif(1)
        }
        if (D <= lambda.s / lambda.star) {
          lambda.star <- lambda.s
          lambda.max <- ifelse(lambda.max > lambda.star,
            lambda.max, lambda.star
          )
          break
        }
        lambda.star <- lambda.s
        lambda.max <- ifelse(lambda.max > lambda.star, lambda.max, lambda.star)
      }

      i <- i + 1
    }
  } else {
    if (is.null(end)) {
      stop("Specify either endtime or n to simulate events. ")
    } else {
      if (verbose == TRUE) {
        message("Simulating up to endtime. To simulate n events specify n.")
      }
    }
    repeat {
      if (ti > end) {
        break
      }

      lambda.star <- lambda.star + alpha
      t <- append(t, ti)

      repeat{
        U <- runif(1)
        while (U == 0) {
          U <- runif(1)
        }
        s <- s - log(U) / lambda.star
        ti <- start + s
        lambda.s <- lambda0 +
          alpha * sum(exp(-beta * c(rep(ti, i) -
            t[1:i], rep(ti, j0 - 1) -
            history[1:j0 - 1])))
        D <- runif(1)
        while (D == 0) {
          D <- runif(1)
        }
        if (D <= lambda.s / lambda.star) {
          lambda.star <- lambda.s
          lambda.max <- ifelse(lambda.max > lambda.star,
            lambda.max, lambda.star
          )
          break
        }
        lambda.star <- lambda.s
        lambda.max <- ifelse(lambda.max > lambda.star, lambda.max, lambda.star)
      }

      i <- i + 1
    }
  }
  hp$events <- sort(t)

  return(list(events = hp$events, lambda.max = lambda.max))
}
