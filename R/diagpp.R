#' Generate diagnostic tools for different models, 
#' including quantile-quantile plot, ks plot, 
#' raw residual and pearson residual.
#'
#' @param object a social network model
#' @param events event times
#' @param pzt probability for producing compensator (mmhp)
#' @importFrom stats ks.test
#' @return print qq plot and ks plot, and print out pearson and raw residuals.
#' @export

diagpp <- function(object, events, pzt = NULL) {
  UseMethod("diagpp")
}

#' @rdname diagpp
#' @export
diagpp.default <- function(object, events, pzt = NULL) {
  cat("Please input the right model. Select from hp, hpp, and mmhp. ")
}

#' @rdname diagpp
#' @export
diagpp.hp<-function(object, events, pzt = NULL){
  r <- compensator(object, events, pzt)
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
diagpp.mmhp <- function(object, events, pzt = 0.5){
  ### need to compute 
  # pzt
  # time.vec
  # latent.vec
  
  r <- compensator(object, events, pzt)
  qqexp(r)
  ksplot(r)
  #rr=rawresidual(object, events, max(events), time.vec, latent.vec)
  #pr=pearsonresidual(object, events, max(events), time.vec, latent.vec,
  #                   latent_event)
  
  ks <- ks.test(r,"pexp")
  # cat("Raw residual: ", rr, "\n",sep = "")
  # cat("Pearson residual: ", pr, "\n",sep = "")
  print(ks)
}


#' @rdname diagpp
#' @export
diagpp.hpp<-function(object, events, pzt = NULL){
  r <- compensator(object, events, pzt)
  qqexp(r)
  ksplot(r)
  rr <- rawresidual(object, events, end = max(events))
  pr <-pearsonresidual(object, events, end = max(events))
  
  ks <- ks.test(r,"pexp")
  cat("\n","Raw residual: ", rr, "\n",sep = "")
  cat("Pearson residual: ", pr, "\n",sep = "")
  print(ks)
}
