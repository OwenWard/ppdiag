#' Generate diagnostic tools for different models, 
#' including quantile-quantile plot, ks plot, 
#' raw residual and pearson residual.
#'
#' @param object a social network model
#' @param events event times
#' @importFrom stats ks.test
#' @return print qq plot and ks plot, and print out pearson and raw residuals.
#' @export

diagpp <- function(object, events) {
  UseMethod("diagpp")
}

#' @rdname diagpp
#' @export
diagpp.default <- function(object, events) {
  cat("Please input the right model. Select from hp, hpp, and mmhp. ")
}

#' @rdname diagpp
#' @export
diagpp.hp<-function(object, events){
  r <- compensator(object, events)
  qqexp(r)
  ksplot(r)
  rr <- rawresidual(object, events, end = max(events))
  pr <- pearsonresidual(object, events, start=min(events),
                        end = max(events))

  ks <- ks.test(r,"pexp")
  cat("Raw residual: ", rr, "\n",sep = "")
  cat("Pearson residual: ", pr, "\n",sep = "")
  print(ks)
}

#' @rdname diagpp
#' @export
diagpp.mmhp <- function(object, events){
  r <- compensator(object, events)
  qqexp(r)
  ksplot(r)
  rr <- rawresidual(object, events, end = max(events), start = 0)
  pr <- pearsonresidual(object, events, end = max(events), start = 0)
  ks <- ks.test(r,"pexp")
  cat("Raw residual: ", rr, "\n",sep = "")
  cat("Pearson residual: ", pr, "\n",sep = "")
  print(ks)
}


#' @rdname diagpp
#' @export
diagpp.hpp<-function(object, events){
  r <- compensator(object, events)
  qqexp(r)
  ksplot(r)
  rr <- rawresidual(object, events, end = max(events))
  pr <-pearsonresidual(object, events, end = max(events))
  
  ks <- ks.test(r,"pexp")
  cat("\n","Raw residual: ", rr, "\n",sep = "")
  cat("Pearson residual: ", pr, "\n",sep = "")
  print(ks)
}
