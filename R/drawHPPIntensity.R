#' Draw intensity of homogeneous Poisson process
#'
#' Draw the intensity for a homogeneous Poisson process
#'
#' @param hpp object for homogeneous Poisson process
#' @param events event times input
#' @param int_title the plot title
#' @param start start of events
#' @param end end of events
#' @param color a specification for the default plotting color.
#' @param plot_events a boolean indicating whether input events will be plotted
#' @param fit a boolean indicating whether to fit a hpp or
#'  use the passed object
#' @param add whether to add the hpp intensity to an existing plot
#' @param verbose whether to output informative messages as running
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics points
#' @return no return value, intensity plot of homogeneous Poisson process
#' @export
#' @examples
#' pois_y <- pp_hpp(lambda = 1)
#' drawHPPIntensity(pois_y, events = pp_simulate(pois_y, end = 10))
drawHPPIntensity <- function(hpp = NULL,
                             events,
                             int_title =
                               "Homogeneous Poisson Process",
                             start = 0,
                             end = max(events),
                             color = "red",
                             plot_events = TRUE,
                             fit = FALSE,
                             add = FALSE,
                             verbose = FALSE) {
  old_events <- hpp$events
  if (add == FALSE) {
    if (is.null(old_events)) {
      if (is.null(events)) {
        stop("Events must be provided either in the object or in the events argument. ")
      }
      if (fit == TRUE) {
        if (verbose == TRUE) {
          message("Fitting provided events.")
        }
        hpp_obj <- fithpp(events)
        lambda <- hpp_obj$lambda
        n <- hpp_obj$n
      } else {
        if (is.null(hpp)) {
          stop("No object provided, set fit=TRUE to fit the events provided.")
        }
        if (verbose == TRUE) {
          message("Using the hpp object. Set fit=TRUE to fit events provided.")
        }
        lambda <- hpp$lambda
        n <- hpp$n
      }
    } else {
      if (is.null(events)) {
        if (verbose == TRUE) {
          message("No events provided. Using the hpp object.")
        }
        lambda <- hpp$lambda
        n <- hpp$n
        events <- hpp$events
        if (start > 0) {
          start <- min(events)
        }
        end <- max(events)
      } else {
        if (fit == TRUE) {
          if (verbose == TRUE) {
            message("Fitting provided events. Set events=NULL to use the events in object.")
          }
          hpp_obj <- fithpp(events)
          lambda <- hpp_obj$lambda
          n <- hpp_obj$n
        } else {
          if (is.null(hpp)) {
            stop("No object provided, set fit=TRUE to fit the events provided.")
          }
          if (verbose == TRUE) {
            message("Using the hpp object. Set fit=TRUE to fit events provided. ")
          }
          lambda <- hpp$lambda
          n <- hpp$n
        }
      }
    }
    fisher <- 1 / lambda
    plot(c(start, end), c(0, (lambda + fisher) * 3),
      type = "n",
      xlab = "Event Times", ylab = "Intensity",
      main = int_title
    )
    abline(h = lambda, col = color)
    abline(h = lambda + fisher, lty = 2)
    abline(h = lambda - fisher, lty = 2)
    if (plot_events == TRUE) {
      for (i in seq_along(events)) {
        points(x = events[i], y = 0, pch = 1, col = "blue")
      }
    }
    legend("topleft", c("Events", "Fisher Inf."),
      lty = c(NA, 2),
      col = c("blue", "black"),
      pch = c(1, NA),
      cex = 0.75
    )
  }
  else {
    # to add to an already created plot
    lambda <- hpp$lambda
    # plot(c(start,end), c(0,(lambda+fisher)*2), type = "n",
    #      xlab = "event times", ylab = "lambda",
    #      main=int_title)
    graphics::segments(x0 = start, x1 = end, y0 = lambda, col = color)
    if (plot_events == TRUE) {
      for (i in seq_along(events)) {
        points(x = events[i], y = 0, pch = 1, col = "blue")
      }
    }
  }
}
