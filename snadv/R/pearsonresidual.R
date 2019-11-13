#' Compute Pearson residuals for social network models
#'
#' Compute Pearson residuals for social network models with model specified time events or simulated time events
#'
#' @param object social network model contating the parameters
#' @param t vector of event happening time
#' @param termination termination time
#' @param time.vec time segment to calculate the intensity for `numeric` method
#' @param latent.vec the probability of the latent space being in the active state
#' @param latent_event the estimated latent space of the point process model
#'
#' @return the pearson residual
#' @export

pearsonresidual <- function(object, t, termination, time.vec, latent.vec, latent_event) {
  UseMethod("pearsonresidual")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.default <- function(object, t, termination, time.vec, latent.vec, latent_event) {
  cat("please input the right model")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.mmhp <- function(object, t, termination, time.vec, latent.vec, latent_event) {
  N <- length(t)
  est.intensity <- UniMMHPIntensity(object,
    event = list(
      tau = t,
      time_segment = time.vec,
      latent_mean = latent.vec
    ),
    method = "numeric"
  )
  est.intensity.events <- UniMMHPIntensity(object, event = list(
    tau = t,
    z = latent_event
  ), method = "atevent")
  pr <- sum(1 / sqrt(est.intensity.events)) -
    sum(sqrt(est.intensity)) * (time.vec[2] - time.vec[1])
  return(pr)
}
# to be test
