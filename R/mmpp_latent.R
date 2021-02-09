#' Latent process of MMPP
#'
#' @param params parameters of the MMPP, an MMPP object
#' @param events events (not including 0, but assumes start at 0)
#' @param zt inferred latent state of events
#' @return list of the states of the Markov process (z.hat)
#' and the times of the transitions between these times (x.hat).
#' @noRd
#'
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' mmpp_obj <- pp_mmpp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, c = 1.1)
#' ppdiag:::mmpp_latent(params = mmpp_obj, events = c(1, 2, 3, 5), zt = c(2, 1, 1, 2))
#' 
mmpp_latent <- function(params = list(lambda0, c, Q),
                        events,
                        zt, start = 0){
  ### do we need to consider other inputs here?
  
  
  # input pars
  #       interevent: length N
  #       zt: inferred latent state, 1 or 2 , length N
  # output  z.hat: states of Markov process
  #         x.hat: time of each transition of Markov process
  lambda0 <- params$lambda0
  c <- params$c
  Q <- params$Q
  q1 <- Q[1,2]
  q2 <- Q[2,1]
  interevent <- diff(c(start,events))
  N <- length(interevent) #+1
  temp.t <- cumsum(interevent)
  if(length(unique(zt))==1){
    z.hat <- rep(unique(zt),2)
    x.hat <- tail(events,1)
  }else{
    z.hat <- rep(NA,sum(diff(zt)!=0)+1)
    x.hat <- rep(NA,sum(diff(zt)!=0)+1)
    z.hat[1] <- zt[1]
    #x.hat[1] <- 0
    temp.count <- 1
    for(l in 2:N){
      if(zt[l]==1 & zt[l-1]==2){
        temp.count <- temp.count + 1
        x.hat[temp.count] <- temp.t[l]
        z.hat[temp.count] <- 1
      }
      if(zt[l]==2 & zt[l-1]==1){
        temp.count <- temp.count + 1
        x.hat[temp.count] <- temp.t[l-1]
        z.hat[temp.count] <- 2
      }
    }
  }
  if(exists("temp.count")){
    return(list(x.hat = c(x.hat[1:temp.count],
                        tail(temp.t, 1 ) ) + start,
                z.hat = c(z.hat[1:temp.count], 3 - z.hat[temp.count])))
  }else{
    return(list(x.hat = x.hat, z.hat = z.hat))
  }
}