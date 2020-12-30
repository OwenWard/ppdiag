#' Generate diagnostic tools for different point process models, 
#' including quantile-quantile plot, ks plot, 
#' raw residual and pearson residual.
#'
#' @param object a point process model
#' @param events event times
#' @importFrom stats ks.test
#' @importFrom graphics par
#' @importFrom graphics layout
#' @return Display QQ-plot and KS plot, along with printing Pearson
#'  and raw residuals.
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
  layout(mat = matrix(c(1,2), nrow = 1, ncol = 2),
         heights = c(2, 2),
         widths = c(2, 2))
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)
  par(mar = c(2, 2, 1, 1))
  pp_ksplot(r)
  rr <- rawresidual(object, events, end = max(events))
  pr <- pearsonresidual(object, events, end = max(events))
  ks <- ks.test(r,"pexp")
  cat("Raw residual: ", rr, "\n",sep = "")
  cat("Pearson residual: ", pr, "\n",sep = "")
  print(ks)
}

#' @rdname diagpp
#' @export
diagpp.mmhp <- function(object, events){
  r <- compensator(object, events)
  layout(mat = matrix(c(1,2), nrow = 1, ncol = 2), heights = c(2, 2),
         widths = c(2, 2))
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)
  par(mar = c(2, 2, 1, 1))
  pp_ksplot(r)
  rr <- rawresidual(object, events, end = max(events))
  pr <- pearsonresidual(object, events, end = max(events))
  ks <- ks.test(r,"pexp")
  cat("Raw residual: ", rr, "\n",sep = "")
  cat("Pearson residual: ", pr, "\n",sep = "")
  print(ks)
}


#' @rdname diagpp
#' @export
diagpp.hpp<-function(object, events){
  r <- compensator(object, events)
  layout(mat = matrix(c(1,2),nrow = 1, ncol = 2), heights = c(2, 2),
         widths = c(2, 2))
  par(mar = c(2, 2, 1, 1))
  pp_qqexp(r)
  par(mar = c(2, 2, 1, 1))
  pp_ksplot(r)
  rr <- rawresidual(object, events, end = max(events))
  pr <-pearsonresidual(object, events, end = max(events))
  
  ks <- ks.test(r,"pexp")
  cat("\n","Raw residual: ", rr, "\n",sep = "")
  cat("Pearson residual: ", pr, "\n",sep = "")
  print(ks)
}
