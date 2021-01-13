test_that("test rawresidual", {
  
  ## pp obj not correct
  obj <- pp_hpp(lambda = 1)
  class(obj) <- "non-pp"
  expect_output(rawresidual(object = obj,events = c(1,2)),
                 "Please input the right model. Select from hp, hpp and mmhp.")
  
  ## special cases of point process, Poisson
  obj <- pp_hpp(lambda = 0)
  expect_identical(rawresidual(object = obj, events = c(1,2)), 2)
  obj <- pp_hpp(lambda = 1)
  expect_identical(rawresidual(object = obj, events = c(1,2)), 0)
  expect_identical(rawresidual(object = obj, events = c(1,2,2.5)), 0.5)
  expect_message(rawresidual(object = obj, events = c(1,2), 
                                 end = 3),
                 "RR calculated to specified end time.")
  ## special cases for Hawkes
  obj <- pp_hp(lambda = 1, alpha = 0, beta = 1)
  expect_identical(rawresidual(object = obj, events = c(1,2)), 0)
  obj <- pp_hp(lambda = 0, alpha = 0, beta = 1)
  expect_identical(rawresidual(object = obj, events = c(1,2)), 2)
  
  ## special cases for mmhp
  
})
