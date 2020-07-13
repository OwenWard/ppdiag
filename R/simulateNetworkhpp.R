#' Simulate network based on poisson process
#'
#' Simulate network based on poisson process (including all the histoty)
#'
#' @param lambda_matrix parameters for poisson process.
#' @param Z vector indicaating which group each node belongs to.
#' @param history the past event times.
#' @param start start time of the poisson process.
#' @param horizon end time of the poisson process.
#' @importFrom stats runif

#' @return simulated network poisson Process in dataframe format
#' @export

simulateNetworkhpp <- function(lambda_matrix, Z, start,horizon, history){
  
  event_data <- tibble()
  n = length(Z)
  for(i in 1:n){
    edges <- sample(c(1:n)[-i],size = 10)
    for(k in 1:length(edges)){
      j <- edges[k]
      lambda0 <- lambda_matrix[Z[i],Z[j]]
      # simulate events here
      inter <- simulatehpp(lambda0, start, horizon, history)
      current_edges <- tibble(Times = inter)
      current_edges$Send <- i
      current_edges$Rec <- j
      current_edges <- current_edges %>% dplyr::select(Send,Rec,Times)
      event_data <- event_data %>% bind_rows(current_edges)
    }
  }
  
  event_data <-event_data %>% arrange(Times) %>% head()
  
  return(event_data)
  
}