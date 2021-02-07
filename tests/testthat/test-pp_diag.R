test_that("test pp_diag", {
  
  ## pp obj not correct
  obj <- pp_hpp(lambda = 1)
  class(obj) <- "non-pp"
  expect_output(pp_diag(object = obj,events = c(1,2)),
                "Please input the right model. Select from hp, hpp, mmpp and mmhp. ")

  
})