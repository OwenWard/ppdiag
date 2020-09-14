test_that("test hpp", {
  #test fit hpp
  hpp_obj=hpp(lambda = 1, end = 10, n=50)
  sim=simulatehpp(hpp_obj)
  hpp=fithpp(sim)
  expect_type(hpp, "list")
  expect_length(hpp, 5)
  
  #test simulate hpp
  expect_type(sim, "double")
  expect_true(!is.null(sim))
  
})
