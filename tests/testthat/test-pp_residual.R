test_that("test pp_residual", {
  # test type of output
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9, 
  lambda1 = 1.1, alpha = 0.8, beta = 1.2)
  y <- pp_simulate(x, n = 10)
  r <- pp_residual(x, events = y$events)
  expect_type(r, "list")
  
  expect_type(r$raw, "double")
  expect_type(r$pearson, "double")
  
  ## pp obj not correct
  obj <- pp_hpp(lambda = 1)
  class(obj) <- "non-pp"
  expect_output(pp_residual(object = obj,events = c(1,2)),
                "Please input the right model. Select from hp, hpp and mmhp.")
  
  ## special cases for HPP
  obj <- pp_hpp(lambda = 1)
  expect_identical(pp_residual(object = obj, events = c(1,2)), list(raw=0, pearson=0))
  expect_message(pp_residual(object = obj, events = c(1,2), 
                                 end = 3),
                 "RR calculated to specified end time.")

  # ## special cases for HP
  obj <- pp_hp(lambda = 1, alpha = 0, beta = 1)
  expect_identical(pp_residual(object = obj, events = c(1,2)), list(raw=0, pearson=0))

  
})