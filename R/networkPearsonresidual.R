#' Compute network based Pearson residuals for social network models
#'
#' Compute network based Pearson residuals for social network models with model specified time events or simulated time events
#'
#' @param object social network model contating the parameters
#' @param t a dataframe containg three columns:send, rec and event happening time
#' @param termination termination time
#' @param time.vec time segment to calculate the intensity for `numeric` method
#' @param latent.vec the probability of the latent space being in the active state
#' @param latent_event the estimated latent space of the point process model
#'
#' @return a dataframe of three columns: Send, Rec and the corresponsding Pearson residual
#' @importFrom stats integrate
#' @export

pearsonresidual <- function(object, t, termination, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  UseMethod("pearsonresidual")
}

#' @rdname pearsonresidual
#' @export
pearsonresidual.default <- function(object, t, termination, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  cat("Please input the right model. Select from hp and mmhp. ")
}

#' @rdname pearsonresidual
#' @export
networkPearsonresidual.hp <- function(object, t, termination, time.vec = NULL, latent.vec = NULL, latent_event = NULL) {
  network_pearsonresidual = data.frame(matrix(ncol = 3, nrow = nrow(t)))
  x <- c("Send", "Rec","Pearson_residual")
  colnames(network_pearsonresidual) <- x
  for (i in 1:nrow(object)){
    pearson_residual <- pearsonresidual.hp(object[i , "hp_parameters"][[1]],t[i,'Times'][[1]])
    network_pearsonresidual[i,] = c(t[i,'Send'][[1]],t[i,'Rec'][[1]],pearson_residual)
  }
  
  return(network_pearsonresidual)
}