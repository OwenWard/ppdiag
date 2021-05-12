test_that("test intensity", {
  # type of output
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- pp_mmhp(Q,
    delta = c(1 / 3, 2 / 3), lambda0 = 0.9, lambda1 = 1.1,
    alpha = 0.8, beta = 1.2
  )
  y <- pp_simulate(x, n = 10)
  expect_type(pp_intensity.mmhp(x, y), "double")
})
