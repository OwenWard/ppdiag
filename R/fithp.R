#' Fit a Hawkes process with exponential kernel to event data
#'
#' Compute the negative log likelihood parameter values for hawkes process.
#' 
#' @param vec vector containing initial values for the 
#' object parameters (lambda0,alpha,beta) to be optimized.
#' @param events vector containing event times.
#' @param end the end time of event times.
#' 
#' @keywords Internal
#' @noRd


negloglik_hp <- function(vec, events, end = max(events)){
	#transforms input list object into vector so that it can be used in optim 
	object <- list(lambda0 = vec[1], alpha = vec[2], beta = vec[3])
	class(object) <- "hp"
  negloglik(object = object, events = events, end = end)
}




#' Determine the MLE of Hawkes process numerically
#' 
#' 
#' @param vec vector of initial parameter values
#' @param events event times
#' @param end end of observation period starting from 0 (default last event)
#' @importFrom stats constrOptim
#' @return a hp object indicating the maximum 
#' likelihood parameter values (lambda0,alpha,beta) for Hawkes process.
#' This is a non-convex problem and a (unique) solution is not guaranteed.
#' @export
#' @examples
#' hp_obj <- pp_hp(lambda0 = 0.1, alpha = 0.45, beta = 0.5)
#' sims <- pp_simulate(hp_obj, start = 0, n = 10)
#' fithp(sims)                  
fithp <- function(events, end = max(events), vec = c(0.1, 0.2, 0.3)){
  con_mat <- matrix(0, nrow = 4, ncol = 3)
  con_mat[1:3, 1:3] <- diag(1, nrow = 3) # constraint all positive
  con_mat[4,] <- c(0, -1, 1) # constrain beta > alpha
  const <- rep(0, 4)
	hawkes.par <- constrOptim(vec, f = negloglik_hp, 
                    events = events, end = end, 
                    ui = con_mat,
                    ci = const,
                    method = "Nelder-Mead")
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
        hawkes.par <- constrOptim(par = vec, fn = negloglik_hp, 
                         events = events, end = max(events), 
                         control = list(maxit = 1000),
                         ui = con_mat,
                         ci = const,
                         method = "Nelder-Mead")
        a <- hawkes.par$par[2]
        b <- hawkes.par$par[3]
        c <- hawkes.par$convergence[1]
      }
      else{
        #if maxit not reached but a>=b, 
        # then we tell the user that we are refitting
        message("A stationary hawkes process requires alpha < beta. 
                Now refitting.")
        hawkes.par <- constrOptim(par = vec, fn = negloglik_hp, 
                                  events = events, end = max(events), 
                                  control = list(maxit = 1000),
                                  ui = con_mat,
                                  ci = const,
                                  method = "Nelder-Mead")
        a <- hawkes.par$par[2]
        b <- hawkes.par$par[3]
        c <- hawkes.par$convergence[1]
      }
      i <- i + 1
      if(i>10){
        stop("Refitting exceeded 10 times. Try a different initial vector. ")
      }
    }
    hp_object <-  list(lambda0 = hawkes.par$par[1], 
                       alpha = hawkes.par$par[2],
                       beta = hawkes.par$par[3],
                       events = events)
    class(hp_object) <- "hp"
    return (hp_object)
}
