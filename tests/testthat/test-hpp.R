test_that("test hpp", {
  #test fit hpp
  hpp_obj <- hpp(lambda = 1, n=50)
  sim <- simulatehpp(hpp_obj, end=10)
  hpp <- fithpp(sim)
  expect_type(hpp, "list")
  expect_length(hpp, 5)
  
  #test simulate hpp
  expect_type(sim, "double")
  expect_true(!is.null(sim))
  
  #test for edge cases
  #start>end
  hpp_obj <- hpp(lambda = 1)
  sim <- simulatehpp(hpp_obj, start=10, end=9)
  expect_type(sim, "double")
  expect_true(!is.null(sim))
  
  #n=0
  hpp_obj <- hpp(lambda = 1,n=0)
  sim <- simulatehpp(hpp_obj, start=10, end=20)
  expect_null(sim)
  
  #test for error messages
  expect_error(simulatehpp(hpp(lambda=1), start=2, end=2),
               "Start and end time identical")
  expect_message(simulatehpp(hpp(lambda=1,n=10), start=2, end=3),
                 "10 events simulated, end time ignored. To simulate up to an endtime don't specify n")
  
})
