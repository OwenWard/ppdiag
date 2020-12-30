#' Simulate point processes
#'
#' @param object point process model object of type hpp, hp, or mmhp
#' @param start start time of events simulated
#' @param end end time of events simulated
#' @param n number of events
#' @return a vector of event times
#' @export
#' @examples
#' hpp_obj=hpp(lambda = 1)
#' s=pp_simulate(hpp_obj, end = 10, n=50)
#' hist(s)
pp_simulate <- function(object, start = 0, end = 1, n = NULL){
  if(class(object) == "hpp"){
      simulatehpp(object, start = start, end = end, n = n, seed = 1)
  }
  
  else if(class(object) == "hp"){
      simulatehp(object, start = start, end = end, history = 0, n = n, seed = 1)
  }
  
  
  else if(class(object) == "mmhp"){
      if (is.null(n)){
        n <- 1
      }
      simulatemmhp(object, n = n, start = start, given_state = FALSE, states = NULL, seed = 1)
  }
  
  else{
    stop("Select the correct point process model from hpp, hp, and mmhp")
  }
}