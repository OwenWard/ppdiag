#' Draw the intensity of the Markov-modulated Hawkes Process(MMHP)
#'
#' Take a mmhp object and draw its intensity accordingly
#'
#' @param mmhp a mmhp object including its state, state_time,
#'  events, lambda0, lambda1, beta and alpha.
#' @param simulation the simulated Markov-modulated Hawkes Process(MMHP)
#' @param int_title title of the plot.
#' @param leg_location location of legend, if moving needed
#' @param color A specification for the default plotting color.
#' @param add logical; if TRUE add to an already existing plot;
#'  if NA start a new plot taking the defaults
#'  for the limits and log-scaling of the x-axis from the previous plot.
#'   Taken as FALSE (with a warning if a different value is supplied)
#'   if no graphics device is open.
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics legend
#' @importFrom graphics segments
#' @return no return value, intensity plot of Markov-modulated Hawkes process
#' @export
#' @examples
#' Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
#' x <- pp_mmhp(Q,
#'   delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1,
#'   alpha = 0.8, beta = 1.2
#' )
#' y <- pp_simulate(x, n = 25)
#' drawUniMMHPIntensity(x, y)
drawUniMMHPIntensity <- function(mmhp, simulation,
                                 int_title = "Intensity of MMHP",
                                 leg_location = "topright",
                                 color = 1,
                                 add = FALSE) {
  # input mmhp: mmhp object
  events <- simulation$events
  event_state <- simulation$zt
  state <- simulation$z
  state_time <- simulation$x
  lambda0 <- mmhp$lambda0
  lambda1 <- mmhp$lambda1
  alpha <- mmhp$alpha
  beta <- mmhp$beta

  n <- length(events)
  m <- length(state)

  ylim <- c()
  for (i in 1:(m - 1)) {
    if (state[i] == 1) {
      hawkes_time <- events[events >= state_time[i] &
        events < state_time[i + 1]]
      if (i == 1) hawkes_time <- hawkes_time[-1]
      history <- events[events < state_time[i]]
      hawkes_obj <- list(
        lambda0 = lambda1,
        alpha = alpha,
        beta = beta
      )
      if (length(hawkes_time) > 1) {
        ylim <- append(ylim, hawkes_max_intensity(hawkes_obj, hawkes_time))
      }
    }
  }

  if (!is.null(ylim)) {
    yupper <- max(ylim)
  } else {
    message("No events in Hawkes state.")
    yupper <- lambda0 + 1
  }

  if (add == FALSE) {
    plot(0, 0,
      xlim = c(0, state_time[m]), ylim = c(0, yupper * 2), type = "n",
      xlab = "Time", ylab = "Intensity",
      main = int_title
    )
    ## should be related to state
    points(events[-1], rep(lambda0 / 2, n - 1),
      cex = 0.6,
      pch = ifelse(event_state[-1] == 1, 16, 1), col = "blue"
    )
    points(state_time, rep(lambda0, m), cex = 0.6, pch = 4, col = "red")
  }
  for (i in 1:(m - 1)) {
    if (state[i] == 1) {
      hawkes_time <- events[events >= state_time[i] &
        events < state_time[i + 1]]
      if (i == 1) {
        hawkes_time <- hawkes_time[-1]
      }
      history <- events[events < state_time[i]]
      hawkes_obj <- list(
        lambda0 = lambda1,
        alpha = alpha,
        beta = beta
      )
      drawHPIntensity(
        hp = hawkes_obj,
        start = state_time[i],
        end = state_time[i + 1],
        history = history[-1],
        events = hawkes_time,
        color = color, i, add = TRUE
      )
      ###
      segments(x0 = state_time[i], y0 = lambda0, y1 = lambda1)
      ###
    } else {
      segments(
        x0 = state_time[i], x1 = state_time[i + 1], y0 = lambda0,
        lty = 2, col = color
      )
    }
  }
  if (add == FALSE) {
    legend(leg_location, c(
      "Hawkes event", "Poisson event",
      "state change point"
    ),
    col = c("blue", "blue", "red"),
    pch = c(16, 1, 4), cex = 0.6
    )
  } else {
    legend(leg_location, c("True", "Estimation"),
      col = c("black", color),
      lty = c(1, 1), cex = 0.6
    )
  }
}
