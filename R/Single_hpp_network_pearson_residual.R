#' print a pearson residual plot for fitting single poisson process to a network
#'
#' 
#' @param t network describing in a datatable containing the sender, receiver and event times.
#' @param n_node number of nodes in the network
#' @param lambda0 the common hp parameter lambdaa across all the pairs
#' @param start_time the starting time of the events 
#' @param end_time the ending time of the events
#' @return a pearson residual plot for fitting a single poisson process to a network
#' @export



network_pearsonresidual.hpp  <- function(t,n_node,lambda0, termination,start_time,end_time){
  mat = matrix(, nrow = n_node, ncol = n_node)
  t  = t%>%group_by(Send,Rec)%>% summarise(Times =list(Times))
  for (i in 1:nrow(t)){
    time <- t[i,'Times'][[1]]
    sender  <- t[i,'Send'][[1]]
    receiver  <- t[i,'Rec'][[1]]
    object <- hpp(lambda = lambda0, start=start_time, end = end_time)
    mat[sender,receiver] <- pearsonresidual.hpp(object, time, termination)
  }
  bk_blue <- c(seq(min(as.numeric(unlist(mat)),na.rm=T),0,by=10))
  bk_red <- c(seq(0,max(as.numeric(unlist(mat)),na.rm=T),by=10))
  bk = c(bk_blue,bk_red)
  mycols <- c(colorRampPalette(colors = c("blue","white"))(length(bk_blue)-1), 'white', colorRampPalette(colors = c("white","red"))(length(bk_red)-1))
  matrix.heatmap(mat,col=mycols, breaks=bk, scale="none")
}

