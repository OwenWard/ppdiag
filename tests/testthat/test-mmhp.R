test_that("test mmhp", {
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9,
            lambda1 = 1.1, alpha = 0.8, beta = 1.2)
  expect_type(x, "list")
  expect_gte(length(x), 7)
  expect_true(!is.null(simulatemmhp(x, n = 10)$events))
  
  # test alpha >= beta
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  expect_error(pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1,
                       alpha = 1.2, beta = 0.8),"Require alpha less than beta for a stationary process")
  
  #Q is null
  Q <- NULL
  x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1,
               alpha = 0.8, beta = 1.2)
  expect_error(simulatemmhp(x, n = 10),
               "No Q matrix specified")
  
  #events already in the object
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1,
               alpha = 0.8, beta = 1.2)
  x$events <- c(0,1,2)
  expect_message(simulatemmhp(x, n = 10),"Events in the mmhp object will be overwritten by simulated events.")
  
})
