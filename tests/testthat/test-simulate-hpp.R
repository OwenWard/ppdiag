test_that("simulate hpp", {
  hpp_obj=hpp(lambda = 1, end = 10, n=50)
  sim=simulatehpp(hpp_obj)
  expect_type(sim, "double")
  expect_true(!is.null(sim))
  
})
