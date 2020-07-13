#' Fit multiple poisson processes to a network
#'
#' 
#' @param t network describing in a datatable containing the sender, receiver and event times.
#' @return a list of hpp object indicating the maximum likelihood parameter values (lambda0) for poisson process plus the sender and receiver information.
#' @export

fit.network.hpp <- function(t){
  t  = t%>%group_by(Send,Rec)%>% summarise(Times =list(Times))
  network_hpp_object <- data.frame(matrix(ncol = 3, nrow = nrow(t)))
  x <- c("Send", "Rec","hpp_parameters")
  colnames(network_hpp_object) <- x
  for (i in 1:nrow(t)){
    hpp_object = fithpp(t[i,'Times'][[1]])
    network_hpp_object[i , "Send"] <- t[i,'Send'][[1]]
    network_hpp_object[i , "Rec"] <- t[i,'Rec'][[1]]
    network_hpp_object[i , "hpp_parameters"][[1]] <- list(hpp_object)
  }
  class(network_hpp_object) = "network_hpp"
  return (network_hpp_object)
}



