#' Interpolate latent process of MMHP
#'
#' @param params parameters of the MMHP, an MMHP object
#' @param events events (not including 0, but assumes start at 0)
#' @param zt inferred latent state of events
#' @param initial.state initial state, if given
#' @param termination.time termination time, if given
#' @param termination.state termination state, if given
#' @param default.inactive default inactive state, 2
#' @importFrom utils tail
#' @return list of the states of the Markov process (z.hat)
#' and the times of the transitions between these times (x.hat).
#' @noRd
#'
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' mmhp_obj <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1,
#'  alpha = 0.8, beta = 1.2)
#' interpolate_mmhp_latent(params = mmhp_obj, events = c(1, 2, 3, 5),
#' zt = c(2, 1, 1, 2))
interpolate_mmhp_latent <- function(params, 
                                        events, zt, 
                                        initial.state = NULL,
                                        termination.time = NULL,
                                        termination.state = NULL,
                                        default.inactive = 2){
  
  interevent <- diff(c(0, events))
  N <- length(interevent)
  inactive_state <- setdiff(unique(zt), c(1))
  
  # specify q1 and q2
  q1 <- params$Q[1, 2]
  q2 <- params$Q[2, 1]
  
  if(params$alpha < 0){
    params$alpha <- abs(params$alpha)
  }
  if(length(unique(zt))==1){
    ## no change at all
    z.hat <- rep(unique(zt),2)
    x.hat <- tail(events,1)
    
    ## termination state
    
    if(!is.null(termination.time)){
      if(is.null(termination.state)){
        if(unique(zt)==1){
          ## check whether state can change between last event and
          # termination time, if change
          ## helper variables
          A.m <- cumsum(exp(params$beta*events)) 
          frequent.par <- q2 - q1 + 
            params$lambda0 - params$lambda1
          
          if(frequent.par<=0){
            x.hat[1] <- tail(events,1) 
            z.hat[2] <- default.inactive
            x.hat[2] <- termination.time
            z.hat[3] <- default.inactive
          }else{
            if(is.finite(A.m[N])){
              l_0 <- params$alpha/params$beta * A.m[N] * 
                exp(-params$beta * events[N])
              l_Delta <- frequent.par * (termination.time-events[N]) +
                params$alpha/params$beta * A.m[N] * 
                exp(-params$beta*termination.time)
              if(l_0>l_Delta){
                x.hat[1] <- tail(events,1) 
                z.hat[2] <- default.inactive
                x.hat[2] <- termination.time
                z.hat[3] <- default.inactive
              }
            }
          }
        }
      }else{
        x.hat[1] <- tail(events,1) 
        z.hat[2] <- termination.state
        x.hat[2] <- termination.time
        z.hat[3] <- termination.state
      }
    }
    
    ## initial state
    if(!is.null(initial.state)){
      if(initial.state!=unique(zt)){
        ## check whether state can change between 0 and first event time, if change
        # initial.delta <- events[1]/2 #TODO
        # x.hat <- c(initial.delta,x.hat)
        # z.hat <- c(initial.state,z.hat)
        
        ## if not change
      }
    }
  }else{
    z.hat <- rep(NA, sum(diff(zt)!=0) + 1)
    x.hat <- rep(NA, sum(diff(zt)!=0) + 1)
    
    ## helper variables
    A.m <- cumsum(exp(params$beta*events)) 
    #length = n; A=alpha/beta*A.m
    frequent.par <- q2 - q1 + 
      params$lambda0 - params$lambda1
    
    z.hat[1] <- zt[1]
    #x.hat[1] <- 0
    temp.count <- 1
    for(l in 2:N){
      if(zt[l]==1 & zt[l-1]==inactive_state){ #inactive change to active
        if(frequent.par <= 0){
          x.hat[temp.count] <- events[l-1]
        }else{
          temp.delta <- 1/params$beta*log(frequent.par/(A.m[l-1] * 
                                                          params$alpha))
          if(events[l-1]+temp.delta>=0){
            x.hat[temp.count] <- events[l-1]
          }else if(events[l]+temp.delta<=0){
            x.hat[temp.count] <- events[l]
          }else{
            x.hat[temp.count] <- -temp.delta
          }
        }
        z.hat[temp.count+1] <- 1
        temp.count <- temp.count + 1
      }
      
      if(zt[l]==inactive_state & zt[l-1]==1){ #active change to inactive
        if(frequent.par<=0){
          x.hat[temp.count] <- events[l-1]
        }else{
          l_0 <- params$alpha/params$beta * A.m[l-1] * 
            exp(-params$beta*events[l-1])
          l_Delta <- frequent.par*(events[l] - events[l-1]) + 
            params$alpha/params$beta * A.m[l-1] * 
            exp(-params$beta*events[l])
          ##
          if(is.finite(A.m[l])){ 
            # added this if and corresponding else below
            if(l_0>l_Delta){
              x.hat[temp.count] <- events[l-1]
            }else{
              x.hat[temp.count] <- events[l]
            }
          }else{
            x.hat[temp.count] <- events[l-1] 
          }
          
        }
        
        z.hat[temp.count+1] <- inactive_state
        temp.count <- temp.count + 1
      }
    }
    
    ## termination
    if(is.null(termination.time)){
      x.hat[temp.count] <- tail(events,1)
      z.hat[temp.count+1] <- z.hat[temp.count]
    }else{
      if(is.null(termination.state)){
        if(z.hat[temp.count]==1){
          ## check whether state can change between last event 
          # and termination time, if change
          if(frequent.par<=0){
            x.hat[temp.count] <- tail(events,1)
            z.hat[temp.count + 1] <- inactive_state
            x.hat[temp.count + 1] <- termination.time
            z.hat[temp.count + 2] <- inactive_state
          }else{
            l_0 <- params$alpha/params$beta * A.m[N] * 
              exp(-params$beta*events[N])
            l_Delta <- frequent.par * (termination.time - events[N]) + 
              params$alpha/params$beta*A.m[N] * 
              exp(-params$beta*termination.time)
            if(is.finite(A.m[N])){
              if(l_0>l_Delta){
                x.hat[temp.count] <- tail(events,1)
                z.hat[temp.count + 1] <- inactive_state
                x.hat[temp.count + 1] <- termination.time
                z.hat[temp.count + 2] <- inactive_state
              }else{
                x.hat[temp.count] <- termination.time
                z.hat[temp.count + 1] <- z.hat[temp.count]
              }
            }else{
              x.hat[temp.count] <- termination.time
              z.hat[temp.count+1] <- z.hat[temp.count]
            }
          }
        }else{
          x.hat[temp.count] <- termination.time
          z.hat[temp.count + 1] <- z.hat[temp.count]
        }
      }else{
        x.hat[temp.count] <- termination.time
        z.hat[temp.count + 1] <- termination.state
      }
    }
    
    ## initial
    if(!is.null(initial.state)){
      if(initial.state!=zt[1]){
        ## check whether state can change between
        # 0 and first event time, if change
        # initial.delta <- events[1]/2 
        # x.hat <- c(initial.delta, x.hat)
        # z.hat <- c(initial.state, z.hat)
        # message("Not completed.")
        ## if not change
      }
    }
  }
  return(list(x.hat=x.hat,z.hat=z.hat))
}

