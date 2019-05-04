#' Simulate Hawkes process during active state
#'
#' Simulate Hawkes process during active state (including all the histoty), a helper function for "simulation.mmhp"
#'
#' @param lambda0 parameters for Hawkes process.
#' @param beta parameters for Hawkes process.
#' @param alpha parameters for Hawkes process.
#' @param history the past event times.
#' @param start start time of the Hawkes process.
#' @param horizon end time of the Hawkes process.

#' @return simulated Hawkes Process
#' @export
simulatehp <- function(lambda0, alpha, beta, start, horizon, history){
  j0 <- length(history) + 1
  lambda.star <- ifelse(j0 == 2, lambda0, lambda0 + alpha * sum(exp(-beta * (rep(start,j0 - 2) - history[2:(j0 - 1)]))))
  lambda.max <- lambda.star
  t <- numeric(10)
  n <- 1
  U <- runif(1)
  s <- -log(U) / lambda.star
  ti <- start + s
  repeat {
    if (ti > horizon){
      break
    }

    lambda.star <- lambda.star + alpha
    t[n] <- ti
    if (length(t) < n + 1) t <- c(t, numeric(10))

    repeat{
      U <- runif(1)
      s <- s - log(U) / lambda.star
      ti <- start + s
      lambda.s <- lambda0 + alpha * sum(exp(-beta * c(rep(ti, n) - t[1:n], rep(ti, j0 - 1) - history[1:j0 - 1])))
      D <- runif(1)
      if(D <= lambda.s / lambda.star){
        lambda.star <- lambda.s
        lambda.max <- ifelse(lambda.max > lambda.star, lambda.max, lambda.star)
        break
      }
      lambda.star <- lambda.s
      lambda.max <- ifelse(lambda.max > lambda.star, lambda.max, lambda.star)
    }

    n <- n + 1
  }

  return(list(t = t[1:(n-1)], lambda.max = lambda.max))
}
