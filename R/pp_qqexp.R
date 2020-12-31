#' Plot QQ-plot for rescaled-inter-event-times of fitted point process
#'
#' Generate Quantile-quantile plot for rescaled-inter-event-times,
#' which are independently 
#' and identically distributed as exponential random variables with rate 1
#' under the true point process.
#'
#' @param r rescaled-inter-event-times
#' @param ... other arguments for plots
#' @importFrom stats ppoints
#' @importFrom stats quantile
#' @importFrom stats qexp
#' @importFrom stats qqline

#' @export



pp_qqexp <- function(r, ...) {
  if(is.null(r)) {
    stop("No rescaled interevent times provided")
  }
  if(length(r) == 1) {
    if(r == 0) {
      stop("No rescaled interevent times provided") 
    }
  }
  if(min(r) < 0) {
    stop("Incorrect interevent times provided")
  }
  p <- ppoints(100) # 100 equally spaced points on (0,1), excluding endpoints
  q <- quantile(r, p = p) # percentiles of the sample distribution
  max_range <- max(c(max( qexp(p) ), q))
  plot(qexp(p), q,
    main = "Q-Q Plot",
    xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", 
    xlim = c(0,max_range),
    ylim = c(0,max_range), ...
  )
  qqline(q, distribution = qexp, col = "blue", lty = 2)
}
