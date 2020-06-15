#' max intensity of a hawkes process
#' @param object parameters for Hawkes process.
#' @param events parameters for Hawkes process.
#'
#' @return max of intensity
#' @export
#' 
hawkes_max_intensity <- function(object, events) {
  r <- 0
  r.max <- 0
  N <- length(events)
  for(i in 2:N){
    r <- exp(-1*object$beta*(events[i]-events[i-1]))*(1+r)
    if(r > r.max) {
      r.max <- r
    } 
  }
  yupper <- object$lambda0 + object$alpha*r.max + object$alpha
  return(yupper)
}