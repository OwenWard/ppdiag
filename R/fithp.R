#' Fit a hawkes process using negloglik and optim functions
#'
#' Compute the maximum likelihood parameter values for hawkes process.
#' 
#' @param vec vector containing initial values for the object parameters (lambda0,alpha,beta) to be optimized.
#' @param t vector containing event times.
#' @param termination the end time of event times.
#' @importFrom stats optim
#' 
#' @return a hp object indicating the maximum likelihood parameter values (lambda0,alpha,beta) for hawkes process.
#' @export



negloglik_hp_new<-function(vec,t,termination){
	#transforms input list object into vector so that it can be used in optim 
	object=list(lambda0=vec[1], alpha=vec[2], beta=vec[3])
	class(object) = "hp"
    negloglik(object=object, t=t, termination=termination)
}


#' Fit a hawkes process using negloglik and optim functions
#'
#' Compute the maximum likelihood parameter values for hawkes process.
#' 
#' @param vec vector containing initial values for the object parameters (lambda0,alpha,beta) to be optimized.
#' @param t vector containing event times.
#' @param termination the end time of event times.
#' @importFrom stats optim
#' 
#' @return a hp object indicating the maximum likelihood parameter values (lambda0,alpha,beta) for hawkes process.
#' @export

fithp<-function(vec,t,termination){
  if(is.null(termination)) {
    termination = max(t)
  }
	hawkes.par=optim(par=vec, fn=negloglik_hp_new, 
                    t=t, termination=termination, control = list(maxit = 1000),lower = c(1e-4,1e-4,1e-4),
                  method = "L-BFGS-B")
    hp_object = list(lambda0=hawkes.par$par[1], alpha=hawkes.par$par[2], beta=hawkes.par$par[3])
    class(hp_object) = "hp"
    return (hp_object)
}
