#' Simulate Hawkes process during active state
#'
#' Simulate Hawkes process during active state (including all the histoty),
#'  a helper function for "simulationmmhp"
#'
#' @param hp hawkes process object, including parameters in list type 
#' (lambda0, alpha, beta, tau)
#' @param history the past event times.
#' @param start start time of the Hawkes process.
#' @param end end time of the Hawkes process.
#' @param n number of events
#' @return simulated Hawkes Process
#' @export
#' @examples
#' hp_obj <- hp(lambda0 = 0.1,alpha = 0.45,beta = 0.5)
#' simulatehp(hp_obj,start = 0, end = 100, n=10)

simulatehp <- function(hp, start=0, end, history=0, n=NULL) {
  events <- hp$events
  if(!is.null(events)){
    stop("Event time already in the hp object.")
  }
  lambda0 <- hp$lambda0
  alpha <- hp$alpha
  beta <- hp$beta
  if(alpha >= beta){
    stop("Stationary hawkes process requires alpha<beta.")
  }
  if(start >= end){
    stop("Start time should be less than end time.")
  }
  j0 <- length(history) + 1
  lambda.star <- ifelse(j0 == 2, lambda0, lambda0 + 
                          alpha * sum(exp(-beta * (rep(start, j0 - 2) - 
                                                     history[2:(j0 - 1)]))))
  lambda.max <- lambda.star
  t <- numeric(10)
  i <- 1
  U <- runif(1)
  s <- -log(U) / lambda.star
  ti <- start + s
  
  if(!is.null(n)){
    repeat {
      if (i > n) {
        break
      }
      
      lambda.star <- lambda.star + alpha
      t[i] <- ti
      if (length(t) < i + 1) t <- c(t, numeric(10))
      
      repeat{
        U <- runif(1)
        s <- s - log(U) / lambda.star
        ti <- start + s
        lambda.s <- lambda0 + 
          alpha * sum(exp(-beta * c(rep(ti, i) - 
                                      t[1:i], rep(ti, j0 - 1) - 
                                      history[1:j0 - 1])))
        D <- runif(1)
        if (D <= lambda.s / lambda.star) {
          lambda.star <- lambda.s
          lambda.max <- ifelse(lambda.max > lambda.star,
                               lambda.max, lambda.star)
          break
        }
        lambda.star <- lambda.s
        lambda.max <- ifelse(lambda.max > lambda.star, lambda.max, lambda.star)
      }
      
      i <- i + 1
    }
    message(paste(n,"events simulated. To simulate up to endtime set n=NULL."))
  }else{
    repeat {
      if (ti > end) {
        break
      }
      
      lambda.star <- lambda.star + alpha
      t[i] <- ti
      if (length(t) < i + 1) t <- c(t, numeric(10))
      
      repeat{
        U <- runif(1)
        s <- s - log(U) / lambda.star
        ti <- start + s
        lambda.s <- lambda0 + 
          alpha * sum(exp(-beta * c(rep(ti, i) - 
                                      t[1:i], rep(ti, j0 - 1) - 
                                      history[1:j0 - 1])))
        D <- runif(1)
        if (D <= lambda.s / lambda.star) {
          lambda.star <- lambda.s
          lambda.max <- ifelse(lambda.max > lambda.star,
                               lambda.max, lambda.star)
          break
        }
        lambda.star <- lambda.s
        lambda.max <- ifelse(lambda.max > lambda.star, lambda.max, lambda.star)
      }
      
      i <- i + 1
    }
    message("Simulating up to endtime. To simulate n events specify n.")
  }

  # t <- round(t,3)
  return(list(t = t, lambda.max = lambda.max))
}
