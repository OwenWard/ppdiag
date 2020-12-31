test_that("raw residual functions", {
  
  ## pp obj not correct
  obj <- hpp(lambda = 1)
  class(obj) <- "non-pp"
  expect_output(rawresidual(object = obj,events = c(1,2)),
                 "Please input the right model. Select from hp, hpp and mmhp.")
  
  ## special cases of point process, Poisson
  obj <- hpp(lambda = 0)
  expect_identical(rawresidual(object = obj, events = c(1,2)), 2)
  obj <- hpp(lambda = 1)
  expect_identical(rawresidual(object = obj, events = c(1,2)), 0)
  expect_identical(rawresidual(object = obj, events = c(1,2,2.5)), 0.5)
  
  ## special cases for Hawkes
  obj <- hp(lambda = 1, alpha = 0, beta = 1)
  expect_identical(rawresidual(object = obj, events = c(1,2)), 0)
  obj <- hp(lambda = 0, alpha = 0, beta = 1)
  expect_identical(rawresidual(object = obj, events = c(1,2)), 2)
  
  ## special cases for mmhp
  
})
