#' Simulate events from a temporal point process
#' 
#' Currently available point processes are homogeneous Poisson,
#' Hawkes with exponential kernel, MMHP and MMPP
#'
#' @param object point process model object of type hpp, hp, mmpp, mmhp, or mmpp
#' @param start start time of events simulated
#' @param end end time of events simulated
#' @param n number of events
#' @return a vector of event times
#' @export
#' @examples
#' hpp_obj <- pp_hpp(lambda = 1)
#' s <- pp_simulate(hpp_obj, n=50)
#' 
#' 
pp_simulate <- function(object, start, end = 1, n = NULL) {
  UseMethod("pp_simulate")
}


#'  @rdname pp_simulate
#'  @export
pp_simulate.default <- function(object, events, start, end = 1, n = NULL) {
  cat("Please input the right model. Select from hp, hpp and mmhp.")
} 


#' @rdname  pp_simulate
#' @export
pp_simulate.hpp <- function(object, start = 0, end = 1, n = NULL) {
  simulatehpp(object, start = start, end = end, n = n)
}


#' @rdname pp_simulate
#' @export
pp_simulate.hp <- function(object, start = 0, end = 1, n = NULL) {
  sim <- simulatehp(object, start = start, end = end,
                    history = NULL, n = n)
  return(sim$events)
}


#' @rdname pp_simulate
#' @export
pp_simulate.mmpp <- function(object, start = 0, end = 1, n = NULL) {
  if (is.null(n)){
    n <- 1
  }
  simulatemmpp(object, n = n, start = start, 
               given_state = FALSE, states = NULL)
}




#' @rdname pp_simulate
#' @export
pp_simulate.mmhp <- function(object, start = 0, end = 1, n = NULL) {
  if (is.null(n)){
    n <- 1
  }
  simulatemmhp(object, n = n, start = start, 
               given_state = FALSE, states = NULL)

}  
  
# pp_simulate <- function(object, start = 0, end = 1, n = NULL){
#   if(class(object) == "hpp"){
#     
#   }
#   
#   else if(class(object) == "hp"){
#     sim <- simulatehp(object, start = start, end = end,
#                  history = NULL, n = n, seed = 1)
#     return(sim$events)
#   }
#   
#   else if(class(object) == "mmpp") {
#     if (is.null(n)){
#       n <- 1
#     }
#     simulatemmpp(object, n = n, start = start, 
#                  given_state = FALSE, states = NULL, seed = 1)
#   }
#   
#   else if(class(object) == "mmhp"){
#     if (is.null(n)){
#       n <- 1
#     }
#     simulatemmhp(object, n = n, start = start, 
#                  given_state = FALSE, states = NULL, seed = 1)
#   }
#   else{
#     stop("Select a point process model from hpp, hp, mmpp and mmhp")
#   }
# }