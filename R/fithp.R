#' Fit a hawkes process using negloglik and optim functions
#'
#' Compute the negative log likelihood parameter values for hawkes process.
#' 
#' @param vec vector containing initial values for the object parameters (lambda0,alpha,beta) to be optimized.
#' @param t vector containing event times.
#' @param end the end time of event times.
#' 
#' @export


negloglik_hp<-function(vec,t,end){
	#transforms input list object into vector so that it can be used in optim 
	object=list(lambda0=vec[1], alpha=vec[2], beta=vec[3])
	class(object) = "hp"
    negloglik(object=object, t=t, end=end)
}




#' Determine the MLE of hawkes process numerically
#' @param vec vector of initial values
#' @param t event times
#' @param end end of observation period
#' @importFrom stats optim
#' @return a hp object indicating the maximum likelihood parameter values (lambda0,alpha,beta) for hawkes process.
#' @export
#' @examples
#' init=rep(0.1,3)
#' hp_obj <- hp(lambda0 = 0.1,alpha = 0.45,beta = 0.5)
#' sims <- simulatehp(hp_obj,start = 0, end = 100, history = 0)
#' fithp(init,sims$t,max(sims$t))                  
fithp<-function(vec,t,end){
	hawkes.par=optim(par=vec, fn=negloglik_hp, 
                    t=t, end=end, control = list(maxit = 1000),lower = c(1e-4,1e-4,1e-4),
                  method = "L-BFGS-B")
    a=hawkes.par$par[2]
    b=hawkes.par$par[3]
    while(TRUE){
	  message("A stationary hawkes process requires alpha<beta. Refitting events to get a stationary hawkes process object.")
	  hawkes.par=optim(par=vec, fn=negloglik_hp, 
	                   t=t, end=end, control = list(maxit = 1000),lower = c(1e-4,1e-4,1e-4),
	                   method = "L-BFGS-B")
	  a=hawkes.par$par[2]
	  b=hawkes.par$par[3]
	  if(a<b){
	    break
	  }
    }
    hp_object = list(lambda0=hawkes.par$par[1], alpha=hawkes.par$par[2], beta=hawkes.par$par[3], t=t)
    class(hp_object) = "hp"
    return (hp_object)
}
