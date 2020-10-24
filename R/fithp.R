#' Fit a hawkes process using negloglik and optim functions
#'
#' Compute the negative log likelihood parameter values for hawkes process.
#' 
#' @param vec vector containing initial values for the 
#' object parameters (lambda0,alpha,beta) to be optimized.
#' @param events vector containing event times.
#' @param end the end time of event times.
#' 
#' @export


negloglik_hp<-function(vec,events,end){
	#transforms input list object into vector so that it can be used in optim 
	object <- list(lambda0=vec[1], alpha=vec[2], beta=vec[3])
	class(object) <- "hp"
  negloglik(object=object, t=events, end=end)
}




#' Determine the MLE of hawkes process numerically
#' @param vec vector of initial parameter values
#' @param events event times
#' @importFrom stats optim
#' @return a hp object indicating the maximum 
#' likelihood parameter values (lambda0,alpha,beta) for hawkes process.
#' @export
#' @examples
#' init=rep(0.3,3)
#' hp_obj <- hp(lambda0 = 0.1,alpha = 0.45,beta = 0.5)
#' sims <- simulatehp(hp_obj,start = 0, end = 10, history = 0, seed=0)
#' fithp(init,sims$events)                  
fithp <- function(vec = rep(0.1, 3), events){
	hawkes.par <- optim(par = vec, fn = negloglik_hp, 
                    events = events, end = max(events), control = list(maxit = 1000),
                    lower = c(1e-4,1e-4,1e-4),
                  method = "L-BFGS-B")
    a <- hawkes.par$par[2]
    b <- hawkes.par$par[3]
    c <- hawkes.par$convergence[1]
    i <- 1
    while(a>=b){
      if(c==51 || c==52){
        message(hawkes.par$message)
      }
      else if(c==1){
        #if maxit reached, then we automatically refit
        hawkes.par <- optim(par=vec, fn=negloglik_hp, 
                         events=events, end=max(events), control = list(maxit = 1000),
                         lower = c(1e-4,1e-4,1e-4),
                         method = "L-BFGS-B")
        a <- hawkes.par$par[2]
        b <- hawkes.par$par[3]
        c <- hawkes.par$convergence[1]
      }
      else{
        #if maxit not reached but a>=b, 
        # then we tell the user that we are refitting
        message("A stationary hawkes process requires alpha<beta. 
                Now refitting.")
        hawkes.par <- optim(par=vec, fn=negloglik_hp, 
                         events=events, end=max(events), control = list(maxit = 1000),
                         lower = c(1e-4,1e-4,1e-4),
                         method = "L-BFGS-B")
        a <- hawkes.par$par[2]
        b <- hawkes.par$par[3]
        c <- hawkes.par$convergence[1]
      }
      i <- i+1
      if(i>10){
        stop("Refitting exceeded 10 times. Try a different initial vector. ")
      }
    }
    hp_object <-  list(lambda0=hawkes.par$par[1], 
                       alpha=hawkes.par$par[2], beta=hawkes.par$par[3], events=events)
    class(hp_object) <- "hp"
    return (hp_object)
}
