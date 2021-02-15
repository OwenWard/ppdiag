#' Estimate Latent process of MMPP
#'
#' @param params parameters of the MMPP, an MMPP object
#' @param events events (not including 0, but assumes start at 0)
#' @param zt inferred latent state of events
#' @return list of the states of the Markov process (z.hat)
#' and the times of the transitions between these times (x.hat).
#' @noRd
#'
#' @examples
#' Q <- matrix(c(-0.04, 0.04, 0.02, -0.02), ncol = 2, byrow = TRUE)
#' mmpp_obj <- pp_mmpp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 1, c = 1)
#' ppdiag:::mmpp_latent(params = mmpp_obj, events = c(1, 2, 3), zt = c(2, 1, 1))
#' 
mmpp_latent <- function(params = list(lambda0, c, Q),
                        events,
                        zt, start = 0){
  # input pars
  #       events: length N
  #       zt: inferred latent state, 1 or 2 , length N
  # output  z.hat: states of Markov process including initial state
  #         x.hat: time of each transition of Markov process
  lambda0 <- params$lambda0
  c <- params$c
  Q <- params$Q
  q1 <- Q[1,2]
  q2 <- Q[2,1]
  interevent <- diff(c(start,events))
  N <- length(interevent) 
  # q2 is prob of 2 -> 1, q1 is prob 1 -> 2 where 2 inactive
  freq_par <- q2 - q1 + lambda0 - lambda0*(1 + c) 
  # print(freq_par)
  if(length(unique(zt))==1){
    z.hat <- rep(unique(zt), 2)
    x.hat <- tail(events, 1)
  }else{
    z.hat <- rep(NA, sum( diff(zt) != 0 ) + 1)
    x.hat <- rep(NA, sum( diff(zt) != 0) ) 
    z.hat[1] <- zt[1]
    temp.count <- 1
    for(l in 2:N){
      # print(x.hat)
      if(zt[l]==1 & zt[l-1]==2){  #0 -> 1
        ##
        if(freq_par < 0) {
          x.hat[temp.count] <- events[l] 
        }
        else if(freq_par > 0) {
          x.hat[temp.count] <- events[l-1] 
        }
        ##
        z.hat[temp.count + 1] <- 1
        temp.count <- temp.count + 1
      }
      if(zt[l]==2 & zt[l-1]==1){ # 1 -> 0
        if( freq_par > 0) {
          x.hat[temp.count] <- events[l]
        }
        else if(freq_par < 0) {
          x.hat[temp.count] <- events[l-1]
        }
        z.hat[temp.count + 1] <- 2
        temp.count <- temp.count + 1
      }
    }
  }
  list(x.hat = x.hat,
       z.hat = z.hat)
}