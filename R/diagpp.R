#' Generate diagnostic tools for different models, including quantile-quantile plot, ks plot, raw residual and pearson residual.
#'
#' @param object a social network model
#' @param t event times
#' @param pzt probability for producing compensator
#' @param time.vec time segment to calculate the intensity for `numeric` method
#' @param latent.vec the probability of the latent space being in the active state
#' @param latent_event the estimated latent space of the point process model
#' @return print qq plot and ks plot, and print out pearson and raw residuals.
#' @export

diagpp <- function(object, t, pzt = NULL, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  UseMethod("diagpp")
}

#' @rdname diagpp
#' @export
diagpp.default <- function(object, t, pzt = NULL, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  cat("Please input the right model. Select from hp and mmhp. ")
}

#' @rdname diagpp
#' @export
diagpp.hp<-function(object, t, pzt = NULL, time.vec = NULL, latent.vec = NULL, latent_event = NULL){
  r=compensator(object, t, pzt)
  qqexp(r)
  ksplot(r)
  rr=rawresidual(object, t, max(t), time.vec, latent.vec)
  paste("The raw residual is", rr)
  pr=pearsonresidual(object, t, max(t), time.vec, latent.vec, latent_event)
  paste("The pearson residual is", pr)
}

#' @rdname diagpp
#' @export
diagpp.mmhp<-function(object, t, pzt = NULL, time.vec = NULL, latent.vec = NULL, latent_event = NULL){
  r=compensator(object, t, pzt)
  qqexp(r)
  ksplot(r)
  rr=rawresidual(object, t, max(t), time.vec, latent.vec)
  paste("The raw residual is", rr)
  pr=pearsonresidual(object, t, max(t), time.vec, latent.vec, latent_event)
  paste("The pearson residual is", pr)
}
