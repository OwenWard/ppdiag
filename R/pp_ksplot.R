#' KS plot of empirical and theoretical cdf curve of fitted point process
#'
#' Plot empirical cdf plot for rescaled-inter-event-times and
#'  exponential cdf as a reference curve
#'
#' @param r rescaled-inter-event-times
#' @param ... other arguments for plots
#' @importFrom stats ecdf
#' @importFrom stats pexp
#' @return no return value, KS plot for rescaled-inter-event-times and exponential cdf curve
#' @export

pp_ksplot <- function(r, ...) {
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
  par <- list(...)
  if ("title" %in% names(par)) {
    title <- par$title
  } else {
    title <- "KSplot"
  }
  if ("xlab" %in% names(par)) {
    xlab <- par$xlab
  } else {
    xlab <- "rescaled residuals"
  }
  if ("ylab" %in% names(par)) {
    ylab <- par$ylab
  } else {
    ylab <- "CDF"
  }
  f <- ecdf(r)
  plot(f, verticals = TRUE, do.points = FALSE,
       xlim = c(min(r), max(r)), main = title, xlab = xlab, ylab = ylab)
  curve(pexp, add = T, col = 2)
  legend("bottomright", c("Exponential CDF", "Empirical CDF"),
    lty = 1, lwd = 2, col = c("Red", "Black"),
    bty = "n", cex = 1.2
  )
}
