#' Compute raw residuals for social network models
#'
#' Compute network based raw residuals for social network models with model specified time events or simulated time events
#'
#' @param object social network model contating the parameters
#' @param t a dataframe containg three columns:send, rec and event happening time
#' @param termination termination time
#' @param time.vec time segment to calculate the intensity for `numeric` method
#' @param latent.vec the probability of the latent space being in the active state
#'
#' @return a dataframe of three columns: Send, Rec and the corresponsding Pearson residual
#' @export


networkRawresidual <- function(object, t, termination, time.vec, latent.vec) {
  UseMethod("rawresidual")
}

#' @rdname rawresidual
#' @export


networkRawresidual.default <- function(object, t, termination, time.vec, latent.vec) {
  cat("Please input the right model. Select from hp and mmhp. ")
}

#' @rdname rawresidual
#' @export

networkRawresidual.hp <- function(object, t, termination, time.vec, latent.vec) {
  network_rawresidual = data.frame(matrix(ncol = 3, nrow = nrow(t)))
  x <- c("Send", "Rec","Raw_residual")
  colnames(network_rawresidual) <- x
  for (i in 1:nrow(object)){
    raw_residual <- rawresidual.hp(object[i , "hp_parameters"][[1]],t[i,'Times'][[1]],termination,time.vec,latent.vec)
    network_rawresidual[i,] = c(t[i,'Send'][[1]],t[i,'Rec'][[1]],raw_residual)
  }
  
  return(network_rawresidual)
}

