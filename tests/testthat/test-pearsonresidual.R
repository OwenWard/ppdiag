test_that("pearson residual special cases", {
  
  ## pp obj not correct
  obj <- pp_hpp(lambda = 1)
  class(obj) <- "non-pp"
  expect_output(pearsonresidual(object = obj,events = c(1,2)),
                "Please input the right model. Select from hp, hpp and mmhp.")
  
  ## special cases of point process, Poisson
  obj <- pp_hpp(lambda = 0)
  expect_identical(pearsonresidual(object = obj, events = c(1,2)), Inf)
  obj <- pp_hpp(lambda = 1)
  expect_identical(pearsonresidual(object = obj, events = c(1,2)), 0)
  expect_message(pearsonresidual(object = obj, events = c(1,2), 
                                 end = 3),
                 "PR calculated to specified end time.")
  # expect_identical(rawresidual(object = obj, events = c(1,2,2.5)), 0.5)
  # 
  # ## special cases for Hawkes
  obj <- pp_hp(lambda = 1, alpha = 0, beta = 1)
  expect_identical(pearsonresidual(object = obj, events = NULL, end = 1), -1)
  expect_identical(pearsonresidual(object = obj, events = c(1,2)), 0)
  obj <- pp_hp(lambda = 0, alpha = 0, beta = 1)
  expect_identical(pearsonresidual(object = obj, events = c(1,2)), Inf)
  
  
  ## special cases for mmhp
  
})