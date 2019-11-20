#' KS plot to visualize the empirical and theoretical cdf curve
#'
#' Plot empirical cdf plot for rescaled-inter-event-times and exponential cdf as a reference curve
#'
#' @param r rescaled-inter-event-times
#' @param ... other arguments for plots
#' @importFrom stats ecdf
#' @importFrom stats pexp

#' @export

ksplot <- function(r, ...) {
  par <- list(...)
  if (title %in% par) {
    title <- title
  } else {
    title <- "KSplot"
  }
  if (xlab %in% par) {
    xlab <- xlab
  } else {
    xlab <- "rescaled residuals"
  }
  if (ylab %in% par) {
    ylab <- ylab
  } else {
    ylab <- "CDF"
  }
  f <- ecdf(r)
  plot(f, verticals = TRUE, do.points = FALSE, xlim = c(min(r), max(r)), main = title, xlab = xlab, ylab = ylab)
  curve(pexp, add = T, col = 2)
  legend("bottomright", c("Exponential CDF", "Empirical CDF"),
    lty = 1, lwd = 2, col = c("Black", "Red"),
    bty = "n", cex = 1.2
  )
}
