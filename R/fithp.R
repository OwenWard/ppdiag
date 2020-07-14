#' Fit a hawkes process using negloglik and optim functions
#'
#' Compute the maximum likelihood parameter values for hawkes process.
#' 
#' @param vec vector containing initial values for the object parameters (lambda0,alpha,beta) to be optimized.
#' @param t vector containing event times.
#' @param end the end time of event times.
#' @importFrom stats optim
#' 
#' @return a hp object indicating the maximum likelihood parameter values (lambda0,alpha,beta) for hawkes process.
#' @export
#' @examples
#' init=rep(0.1,3)
#' sims <- simulatehp(hp_obj,start = 0, end = 100, history = 0)
#' fithp(init,sims$t,max(sims$t))

negloglik_hp<-function(vec,t,end){
	#transforms input list object into vector so that it can be used in optim 
	object=list(lambda0=vec[1], alpha=vec[2], beta=vec[3])
	class(object) = "hp"
    negloglik(object=object, t=t, termination=end)
}

                  
fithp<-function(vec,t,end){
	hawkes.par=optim(par=vec, fn=negloglik_hp, 
                    t=t, termination=end, control = list(maxit = 1000),lower = c(1e-4,1e-4,1e-4),
                  method = "L-BFGS-B")
    hp_object = list(lambda0=hawkes.par$par[1], alpha=hawkes.par$par[2], beta=hawkes.par$par[3])
    class(hp_object) = "hp"
    return (hp_object)
}
