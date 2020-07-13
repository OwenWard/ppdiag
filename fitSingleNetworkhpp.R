#' Fit single poisson process to a network
#'
#' 
#' @param t network describing in a datatable containing the sender, receiver and event times.
#' @return a single value indicating lambda for the single poisson process fitting to the whole network
#' @export

fit.single.network.hpp <- function(t){
  t  = t%>%group_by(Send,Rec)%>% summarise(Times =list(Times))
  hpp_lambda <- 0
  for (i in 1:nrow(t)){
    hpp_lambda <- hpp_lambda+ fithpp(t[i,'Times'][[1]])$lambda
  }
  return (hpp_lambda/nrow(t))
}



