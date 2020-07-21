#' Fit single poisson process to a network
#'
#' 
#' @param t network describing in a datatable containing the sender, receiver and event times.
#' @return a single value indicating lambda for the single poisson process fitting to the whole network
#' @export

fit.single.network.hpp <- function(t){
  n_events = nrow(t)
  t_range = max(t$Times)-min(t$Times)
  t  = t%>%group_by(Send,Rec)%>% summarise(Times =list(Times))
  n_group = nrow(t)
  return (n_events/(t_range*n_group))
}



