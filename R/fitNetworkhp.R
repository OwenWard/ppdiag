#' Fit multiple hawkes processes to a network using negloglik and optim functions
#'
#' Compute the maximum likelihood parameter values for hawkes process.
#' 
#' @param vec vector containing initial values for the object parameters (lambda0,alpha,beta) to be optimized.
#' @param t network describing in a datatable containing the sender, receiver and event times.
#' @param termination the end time of event times.
#' @importFrom stats optim
#' @return a list of hp object indicating the maximum likelihood parameter values (lambda0,alpha,beta) for hawkes process plus the sender and receiver information.
#' @export



fit.network.hp <- function(vec, t, termination){
  t  = t%>%group_by(Send,Rec)%>% summarise(Times =list(Times))
  network_hp_object <- data.frame(matrix(ncol = 3, nrow = nrow(t)))
  x <- c("Send", "Rec","hp_parameters")
  colnames(network_hp_object) <- x
  for (i in 1:nrow(t)){
    hp_object = fithp(vec,t[i,'Times'][[1]],termination)
    network_hp_object[i , "Send"] <- t[i,'Send'][[1]]
    network_hp_object[i , "Rec"] <- t[i,'Rec'][[1]]
    network_hp_object[i , "hp_parameters"][[1]] <- list(hp_object)
  }
  class(network_hp_object) = "network_hp"
  return (network_hp_object)
}

