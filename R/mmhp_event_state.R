#' Estimate the latent state for events of an MMHP
#' 
#' Given a MMHP object and events from that MMHP, infer the
#' most likely state of the Markov Process and each event time, along
#' with the probability of being in the active state.
#'
#' @param params the parameters of the chosen MMHP
#' @param events the event times
#' @param start the start of observation
#'
#' @return probability of being in state 1 (active state) at each event,
#' along with most likely state
#' @noRd
#'
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' mmhp_obj <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9,
#'  lambda1 = 1.1,
#'  alpha = 0.8, beta = 1.2)
#'  ## evaluate at some fake event times
#'  mmhp_event_state(params = mmhp_obj, events = c(1, 2, 3, 5)) 
mmhp_event_state <- function(params = 
                               list(lambda0,
                                    lambda1,
                                    alpha,
                                    beta,
                                    Q),
                             events, 
                             start = 0){
  lambda0 <- params$lambda0
  lambda1 <- params$lambda1
  alpha <- params$alpha
  beta <- params$beta
  Q <- params$Q
  q1 <- Q[1, 2]
  q2 <- Q[2, 1]
  ### case where 0 or 1 events only
  if(is.null(events)) {
    stop("No events provided")
  }
  
  interevent <- diff(c(0,events))
  N <- length(interevent)
  pzt <- rep(0, N)
  zt <- rep(0, N)
  r <- rep(0, N) 
  intensities <- rep(0, 2)
  integrals <- rep(0, 2)
  forward <- matrix(0, nrow = N, ncol = 2)
  backward <- matrix(0, nrow = N, ncol = 2)
  probs_1 <- matrix(0, nrow = N, ncol = 2) 
  #Probability vector for transition to state 1 (active state)
  probs_2 <- matrix(0, nrow = N, ncol = 2) 
  #Probability vector for transition to state 2 (inactive state)
  
  probs_1[,1] <- -q1*interevent
  probs_2[,2] <- -q2*interevent
  probs_1[,2] <- log(1 - exp(probs_2[,2]))
  probs_2[,1] <- log(1 - exp(probs_1[,1]))
  
  #calculate forward and log_p_z_star => zt*[N]
  forward[1,1] <- log(lambda0) - interevent[1]*lambda1
  forward[1,2] <- log(lambda0) - interevent[1]*lambda0 
  #calculate forward variables, uniform initial distribution for latent state
  r[1] <- 0
  if(N > 1 ){
    for(n in 2:N){
      r[n] <- exp(-beta * interevent[n])*(r[n-1] + 1)
      a <- max(forward[n-1,] + probs_1[n,])
      forward[n,1] <- a + log( sum( exp(forward[n-1,] + probs_1[n,] - a) ) ) + 
        log(lambda1 + alpha*exp(-beta * interevent[n]) * (r[n-1] + 1) ) -
        interevent[n]*lambda1 + alpha/beta*(r[n]-r[n-1]-1)
      a <- max(forward[n-1,] + probs_2[n-1,])
      forward[n,2] <- a + log( sum( exp(forward[n-1,] + probs_2[n,] - a) ) ) +
        log(lambda0) - interevent[n]*lambda0
    }
    
    #calculate backward variables
    backward[N,1] <- 0
    backward[N,2] <- 0
    intensities[2] <- lambda0
    
    for(n in 2:N){
      m <- N - n + 1
      intensities[1] <- lambda1 + alpha*exp(-beta*interevent[m+1]) * (r[m] + 1)
      integrals[2] <- lambda0*interevent[m]
      integrals[1] <- lambda1*interevent[m] - alpha/beta*(r[m+1] - r[m] - 1)
      a <-  max(backward[m+1,] + probs_1[m+1,] + log(intensities) - integrals)
      backward[m,1] <- a + log(sum(exp(backward[m+1,] + probs_1[m+1,] + 
                                         log(intensities) - integrals - a)))
      a <-  max(backward[m+1,] + probs_2[m+1,] + 
                  log(intensities) - integrals)
      backward[m,2] <- a + log(sum(exp(backward[m+1,] + probs_2[m+1,] + 
                                         log(intensities) - integrals - a)))
    }
  }
  
  
  #infer the probability of zt=1
  for(n in 1:N){
    pzt[n] <- 1/(1 + exp(forward[n,2] - forward[n,1] + 
                         backward[n,2] - backward[n,1]))
    if(forward[n,2] + backward[n,2] > forward[n,1] + backward[n,1]){
      zt[n] <- 2;
    }else{
      zt[n] <- 1;
    }
  }
  return(list(pzt = pzt, zt = zt))
}

