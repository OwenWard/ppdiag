################################################################################################
#This R file is source code to including functions related to 
#Univariate Hawkes process
# -- uniHawkesSimulation
# -- uniHawkesMaxIntensity
################################################################################################
#------------This function is used to simulate univariate Hawkes process
uniHawkesSimulation<-function(object, horizon=NULL, N=NULL){
  # input object: parameters for Hawkes process, include lambda0, alpha, beta
  #       horizon or Number of events(N)
  # output events: vector of event time
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  lambda.star <- lambda0
  
  if( !is.null(N) ){
    events <- numeric(N)
    U<-runif(1)
    s<--log(U)/lambda.star
    events[1]<-s
    
    for(n in c(2:N)){
      lambda.star<-lambda.star+alpha
      repeat{
        U<-runif(1)
        s<-s-log(U)/lambda.star
        lambda.s<-lambda0+alpha*sum(exp(-beta*(rep(s,n)-events[1:n])))
        D<-runif(1)
        if(D<=lambda.s/lambda.star){
          lambda.star<-lambda.s
          break
        }
        lambda.star<-lambda.s
      }
      events[n]<-s
    }
    return(events)
    
  }else if( !is.null(horizon) ){
    events <- numeric(100)
    n<-1
    U<-runif(1)
    s<--log(U)/lambda.star
    
    repeat{
      
      if (s > horizon){
        break
      } 
      
      lambda.star<-lambda.star+alpha
      events[n]<-s
      if (length(events) < n+1) events <- c(events, numeric(100)) 
      
      repeat{
        U<-runif(1)
        s<-s-log(U)/lambda.star
        lambda.s<-lambda0+alpha*sum(exp(-beta*(rep(s,n)-events[1:n])))
        D<-runif(1)
        if(D<=lambda.s/lambda.star){
          lambda.star<-lambda.s
          break
        }
        lambda.star<-lambda.s
      }
      n <- n + 1
    }
    return(events[1:(n-1)])
  }else{
    stop("Need to define horizon or number of events(N)")
  }
}

#------------This function is to calculate the maximum of intensity
uniHawkesMaxIntensity <- function(object, events) {
  r <- 0
  r.max <- 0
  N <- length(events)
  for(i in 2:N){
    r <- exp(-object$beta*(events[i]-events[i-1]))*(1+r)
    if(r > r.max) r.max <- r
  }
  yupper <- object$lambda0 + object$alpha*r.max + object$alpha
  return(yupper)
}

#------------This function is to compute numerical value of intensity function
uniHawkesIntensityNumeric<-function(object, events, time.vec=NULL){
  if(is.null(time.vec)){
    delta.t <- tail(events,1)/20000
    time.vec <- seq(delta.t,tail(events,1),delta.t)
  }
  lambda.t <- rep(object$lambda0,length(time.vec))
  event.idx <- 2
  start.idx <- sum(time.vec<=events[2])+1
  r <- 0
  for(i in c(start.idx:length(time.vec))){
    current.t <- time.vec[i]
    if(current.t>events[event.idx+1]){
      event.idx <- event.idx + 1
      r <- exp(-object$beta*(events[event.idx]-events[event.idx-1]))*(1+r)
    }
    lambda.t[i]<-object$lambda0+object$alpha*exp(-object$beta*(current.t-events[event.idx]))*(1+r)
  }
  if(is.null(time.vec)){
    return(list(lambda.t=lambda.t,delta.t=delta.t,time.vec=time.vec))
  }else{
    return(list(lambda.t=lambda.t,time.vec=time.vec))
  }
}

#------------This function is used to compute \int_0^T \lambda(u) du
uniHawkesIntegralIntensity <- function(object, events, termination){
  # input object: parameters for Hawkes process, include lambda0, alpha, beta 
  #       events: vector of event happening time
  #       T: termination time
  # output result: \int_0^T \lambda(u) du
  
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  
  N<-length(events)
  r<-0
  
  if(N>1){
    for(i in 2:N){
      r <- exp(-beta*(events[i]-events[i-1]))*(r+1)
    }
  }
  
  if(N==0){
    result <- lambda0*termination
  }else{
    result <- lambda0*termination+alpha/beta*(N-(1+r)*exp(-beta*(termination-events[N])))
  }
  
  return(result)
}

#------------This function is used to compute compensator for univariate Hawkes process
uniHawkesCompensator <- function(object, events){
  # input object: parameters for Hawkes process, include lambda0, alpha, beta 
  #       events: vector of event happening time
  # output Lambda: vector of intensity compensator
  
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  
  N<-length(events)
  Lambda<-rep(0,N)
  r<-0
  Lambda[1]<-lambda0*(events[1])
  for(i in 2:N){
    delta.t <- events[i]-events[i-1]
    temp.r <-exp(-beta*delta.t)*(r+1)
    Lambda[i]<-lambda0*delta.t-alpha/beta*(temp.r-r-1)
    r <- temp.r
  }
  return(Lambda)
}

#------------This function is used to compute Pearson residual for univariate Hawkes process
uniHawkesPearsonResidual <- function(object, events, termination){
  # input object: parameters for Hawkes process, include lambda0, alpha, beta 
  #       events: vector of event happening time
  #       T: termination time
  # output result: Pearson residual
  
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  
  N<-length(events)
  PR <- 0
  r <- 0
  
  if(N == 0){
    return(-sqrt(lambda0)*termination)
  }else if(N == 1){
    PR <- PR + 1/sqrt(lambda0) - sqrt(lambda0)*events[1]
    integrand <- function(u) {sqrt(lambda0+alpha*exp(-beta*(u-events[1])))}
    PR <- PR - integrate(integrand, lower = 0, upper = termination)$value
    return(PR)
  }else{
    # first event
    PR <- PR + 1/sqrt(lambda0) - sqrt(lambda0)*events[1]
    # 2~N events
    for(i in 2:N){
      r <- exp(-beta*(events[i]-events[i-1]))*(r+1)
      if(lambda0+alpha*r>0){
        PR <- PR + 1/sqrt(lambda0+alpha*r)
      }
      
      integrand <- function(u){
        temp <- lambda0
        for(k in c(1:(i-1))){
          temp <- temp + alpha*exp(-beta*(u-events[k]))
        }
        return(sqrt(temp))
      }
      
      PR <- PR - integrate(integrand, lower = events[i-1], upper = events[i])$value
    }
    # N event ~ termination time
    
    integrand <- function(u){
      temp <- lambda0
      for(k in c(1:N)){
        temp <- temp + alpha*exp(-beta*(u-events[k]))
      }
      return(sqrt(temp))
    }
    
    PR <- PR - integrate(integrand, lower = events[N], upper = termination)$value
    return(PR) 
  }
}

#------------This function is calculates the intensity for univariate Hawkes process
uniHawkesIntensity <- function(object, events, current_time) {
  # input object: parameters for Hawkes process, include lambda0, alpha, beta 
  #       events: vector of event happening time
  # output object: intensity value at current time
  lambda0 <- object$lambda0
  alpha <- object$alpha
  beta <- object$beta
  events <- events[events<current_time]
  N <- length(events)
  r <- rep(0,N)
  
  return(lambda0 + alpha*sum(exp(-beta*(rep(current_time,N)-events))))
} 


