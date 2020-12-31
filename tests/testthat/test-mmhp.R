test_that("multiplication works", {
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9,
            lambda1 = 1.1, alpha = 0.8, beta = 1.2)
  expect_type(x, "list")
  expect_gte(length(x), 7)
  
  #test for error messages
  expect_error(simulatemmhp(mmhp(lambda0 = 0.9, lambda1 = 1.1,
                                 alpha = 1, beta = 0.5)),
               "Require alpha less than beta for a stationary process")
})
