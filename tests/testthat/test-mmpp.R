test_that("test mmpp", {
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- pp_mmpp(Q = Q, lambda0 = 1, c = 1.5, delta = c(1/3, 2/3))
  expect_type(x, "list")
  expect_gte(length(x), 5)
  expect_true(!is.null(simulatemmpp(x, n = 10)$events))
  expect_length(simulatemmpp(x, n = 10)$events,11)

  # Q is null
  Q <- NULL
  x <- pp_mmpp(Q = Q, lambda0 = 1, c = 1.5, delta = c(1/3, 2/3))
  expect_error(simulatemmpp(x, n = 10),
               "No Q matrix specified")
  
  # events already in the object
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- pp_mmpp(Q = Q, lambda0 = 1, c = 1.5, delta = c(1/3, 2/3))
  x$events <- c(0,1,2)
  expect_message(simulatemmpp(x, n = 10),"Events in the mmpp object will be overwritten by simulated events.")
  
  # invalid delta
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- pp_mmpp(Q = Q, lambda0 = 1, c = 1.5, delta = c(2/3, 2/3))
  expect_error(simulatemmpp(x, n = 10),"Invalid delta")

  # n given, given_states=FALSE
  # Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  # x <- pp_mmpp(Q = Q, lambda0 = 1, c = 1.5, delta = c(1/3, 2/3))
  # expect_message(simulatemmpp(x, n = 10),"10 events simulated. To simulate up to endtime set given_states=TRUE and provide states.")

  
})
