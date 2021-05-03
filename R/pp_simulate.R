#' Simulate events from a temporal point process
#'
#' Currently available point processes are homogeneous Poisson,
#' Hawkes with exponential kernel, MMHP and MMPP
#'
#' @param object point process model object of type hpp, hp, mmhp, or mmpp
#' @param start start time of events simulated. Not used for Markov modulated
#' models
#' @param end end time of events simulated. Not used for Markov modulated models
#' @param n number of events simulated. Required for Markov modulated models,
#' optional otherwise
#' @param verbose whether to output informative messages as running
#' @importFrom utils capture.output
#' @return a vector of event times for all models. For Markov modulated models,
#' also returns details on the underlying latent process
#' @export
#' @examples
#' hpp_obj <- pp_hpp(lambda = 1)
#' s <- pp_simulate(hpp_obj, n = 50)
pp_simulate <- function(object, start = 0, end = 1,
                        n = NULL, verbose = FALSE) {
  UseMethod("pp_simulate")
}


#' @rdname pp_simulate
#' @export
pp_simulate.default <- function(object, start = 0, end = 1,
                                n = NULL, verbose = FALSE) {
  cat("Please input the right model. Select from hp, hpp and mmhp.")
}


#' @rdname  pp_simulate
#' @export
pp_simulate.hpp <- function(object, start = 0, end = 1,
                            n = NULL, verbose = FALSE) {
  simulatehpp(object, start = start, end = end, n = n, verbose = verbose)
}


#' @rdname pp_simulate
#' @export
pp_simulate.hp <- function(object, start = 0, end = 1,
                           n = NULL, verbose = FALSE) {
  sim <- simulatehp(object,
    start = start, end = end,
    history = NULL, n = n, verbose = verbose
  )
  return(sim$events)
}


#' @rdname pp_simulate
#' @export
pp_simulate.mmpp <- function(object, start = 0, end = 1,
                             n = NULL, verbose = FALSE) {
  if (is.null(n)) {
    message("n a required argument for MMPP. Simulating 10 events")
    n <- 10
  }
  simulatemmpp(object,
    n = n, start = start,
    given_state = FALSE, states = NULL, verbose = verbose
  )
}




#' @rdname pp_simulate
#' @export
pp_simulate.mmhp <- function(object, start = 0, end = 1,
                             n = NULL, verbose = FALSE) {
  if (is.null(n)) {
    message("n a required argument for MMHP. Simulating 10 events")
    n <- 10
  }
  simulatemmhp(object,
    n = n, start = start,
    given_state = FALSE, states = NULL, verbose = verbose
  )
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
