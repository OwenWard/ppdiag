#' Summarise diagnostics for point process models
#'
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
#' @examples 
#' \dontrun{
#' hpp_obj <- pp_hpp(lambda = 1)
#' events <- pp_simulate(hpp_obj, end=10)
#' pp_diag(hpp_obj, events)
#' }

pp_diag <- function(object, events) {
  UseMethod("pp_diag")
}

#' @rdname pp_diag
#' @export
pp_diag.default <- function(object, events) {
  cat("Please input the right model. Select from hp, hpp, mmpp and mmhp. ")
}

#' @rdname pp_diag
#' @export
pp_diag.hp<-function(object, events){
  r <- pp_compensator(object, events)
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

#' @rdname pp_diag
#' @export
pp_diag.mmhp <- function(object, events){
  r <- pp_compensator(object, events)
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

#' @rdname pp_diag
#' @export
pp_diag.mmpp <- function(object, events){
  r <- pp_compensator(object, events)
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

#' @rdname pp_diag
#' @export
pp_diag.hpp<-function(object, events){
  r <- pp_compensator(object, events)
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