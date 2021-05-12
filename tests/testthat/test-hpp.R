test_that("test hpp", {

  # test fit hpp
  hpp_obj <- pp_hpp(lambda = 1)
  sim <- simulatehpp(hpp_obj, end = 10, n = 50)
  hpp <- fithpp(sim)
  expect_type(hpp, "list")
  expect_length(hpp, 2)
  expect_error(fithpp(sim, end = 0))

  # test simulate hpp
  expect_type(sim, "double")
  expect_true(!is.null(sim))

  # test for edge cases
  # start>end
  hpp_obj <- pp_hpp(lambda = 1)
  sim <- simulatehpp(hpp_obj, start = 20, end = 10)
  expect_null(sim)
  # start==end
  sim <- simulatehpp(hpp_obj, start = 20, end = 20)
  expect_null(sim)

  # n=0
  hpp_obj <- pp_hpp(lambda = 1)
  expect_error(
    simulatehpp(hpp_obj, start = 2, end = 10, n = 0),
    "n must be positive for simulation."
  )

  # test for messages
  expect_message(
    simulatehpp(pp_hpp(lambda = 1),
      start = 2, end = 3, n = 9, verbose = TRUE
    ),
    "9 events simulated. To simulate up to an endtime set n=NULL."
  )
  expect_message(
    simulatehpp(pp_hpp(lambda = 1),
      start = 2,
      end = 3, verbose = TRUE
    ),
    "Simulating up to endtime. To simulate n events specify n."
  )
  set.seed(100)
  y <- simulatehpp(pp_hpp(lambda = 1), start = 2, end = 3, n = 9)
  set.seed(100)
  z <- sort(simulatehpp(pp_hpp(lambda = 1),
    start = 2, end = 3,
    n = 9
  ))
  expect_identical(y, z)
})
