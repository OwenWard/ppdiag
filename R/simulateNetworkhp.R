#' Simulate network based on Hawkes process during active state
#'
#' Simulate network based on Hawkes process during active state (including all the histoty), a helper function for "simulationmmhp"
#'
#' @param lambda_matrix parameters for Hawkes process.
#' @param beta_matrix parameters for Hawkes process.
#' @param alpha_matrix parameters for Hawkes process.
#' @param Z vector indicaating which group each node belongs to.
#' @param history the past event times.
#' @param start start time of the Hawkes process.
#' @param horizon end time of the Hawkes process.
#' @importFrom stats runif

#' @return simulated network Hawkes Process in dataframe format
#' @export

simulateNetworkhp <- function(lambda_matrix, alpha_matrix, beta_matrix, Z, start,horizon, history){
  
  event_data <- tibble()
  n = length(Z)
  for(i in 1:n){
    ## why 10?
    edges <- sample(c(1:n)[-i],size = 10)
    for(k in 1:length(edges)){
      j <- edges[k]
      lambda0 <- lambda_matrix[Z[i],Z[j]]
      alpha <- alpha_matrix[Z[i],Z[j]]
      beta <- beta_matrix[Z[i],Z[j]]
      # simulate events here
      inter <- simulatehp(lambda0, alpha, beta, start, horizon, history)$t
      current_edges <- tibble(Times = inter)
      current_edges$Send <- i
      current_edges$Rec <- j
      current_edges <- current_edges %>% dplyr::select(Send,Rec,Times)
      event_data <- event_data %>% bind_rows(current_edges)
    }
  }
  
  event_data <- event_data %>% arrange(Times) %>% head()
  
  return(event_data)
  
}