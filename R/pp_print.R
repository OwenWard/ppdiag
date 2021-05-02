#' Print temporal point process objects
#' 
#' Currently available point processes are homogeneous Poisson,
#' Hawkes with exponential kernel, MMHP and MMPP
#'
#' @param object point process model object of type hpp, hp, mmhp, or mmpp
#' @return no return value, print object attributes
#' @export
#' @examples
#' hpp_obj <- pp_hpp(lambda = 1)
#' pp_print(hpp_obj)
#' 
#' 

#' @rdname pp_print
#' @export
pp_print <- function(object) {
  #get the list of attributes
  attr <- c()
  for (i in attributes(object)){
    for (j in i){
      attr = c(attr, j)
    }
  }
  
  #print attributes
  for (i in attr){
    if(!is.null(object[[i]])){
      cat(paste0(i, ': ', object[[i]], '\n'))
    }
  }
  #print the class of object: hp/hpp/mmhp/mmpp
  cat("class:", attributes(object)$class)
} 


  
