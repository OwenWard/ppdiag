test_that("simulate mmhp", {
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1, alpha = 0.8, beta = 1.2)
  sim=simulatemmhp(x)
  expect_type(sim, "list")
  expect_equal(length(sim), 5)
  expect_true(!is.null(sim$events))
  
})


