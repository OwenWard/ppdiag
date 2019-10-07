#' Return the Pearson residual of the Hawkes Process
#'
#' Take a Hawkes Process object including lambda0, alpha and beta, as well as the event times, then generate its pearson residual
#' @param object parameters for Hawkes process, include lambda0, alpha, beta 
#' @param events vector of event happening time
#' @param termination termination time
#' @importFrom stats integrate
#' @export
uniHawkesPearsonResidual <- function(object, events, termination){
  
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
    PR <- PR - integrate(integrand, lower = events[1], upper = termination)$value
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