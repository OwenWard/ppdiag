test_that("compensator correct objects", {
  obj <- pp_hpp(lambda = 1)
  class(obj) <- "non-pp"
  expect_output(pp_compensator(object = obj,events = c(1, 2)),
                "Please input the right model. Select from hp, hpp and mmhp.")
  expect_identical(pp_compensator(object = pp_hpp(lambda = 1), events = 0), 0)
  expect_identical(pp_compensator(object = pp_hpp(lambda = 1), 
                                  events = c(0,1)), 1)
  ## need to deal with cases with only one or two events here also
  ## for mmhp
  expect_identical(pp_compensator(object = pp_hp(lambda = 1,
                                                 alpha = 0, 
                                                 beta = 1),
                                  events = c(0,1)), c(1))
  expect_identical(pp_compensator(object = pp_hp(lambda = 1,
                                                 alpha = 0, 
                                                 beta = 1),
                                  events = c(0,1,2)), c(1,1))
  expect_identical(pp_compensator(object = pp_hp(lambda = 1,
                                                 alpha = 0, 
                                                 beta = 1),
                                  events = c(0)), 0)
})
