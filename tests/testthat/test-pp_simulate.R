test_that("test pp_simulate", {
  
  ## tests for hpp
  expect_message(pp_simulate(pp_hpp(lambda = 1), start = 2,
                             end = 3, n = 5, verbose = TRUE),
                 "5 events simulated. To simulate up to an endtime set n=NULL.")
  expect_message(pp_simulate(pp_hpp(lambda = 1), start = 2, end = 3, verbose = TRUE),
                 "Simulating up to endtime. To simulate n events specify n.")
  expect_message(pp_simulate(pp_hpp(lambda = 1, events = c(1)),
                             start = 2, 
                             end = 3, verbose = TRUE),
                 "Events in the hpp object will be overwritten by simulated events.")
  expect_null(pp_simulate(pp_hpp(lambda = 1), start = 0, end = -1))
  expect_null(pp_simulate(pp_hpp(lambda = 1), start = 0, end = 0))
  expect_error(pp_simulate(pp_hpp(lambda = 1), n = 0),
               "n must be positive for simulation")
  expect_error(pp_simulate(pp_hpp(lambda = 1), n = -2),
               "n must be positive for simulation")
  expect_error(pp_simulate(pp_hpp(lambda = 1), start = 0, end = NULL),
               "Specify either endtime or n to simulate events")
  
  ## tests for hp
  hp_obj <- pp_hp(1.5, 0.1, 0.2)
  expect_message(pp_simulate(hp_obj, start = 2, end = 3, n = 10, verbose = TRUE),
                 "10 events simulated. To simulate up to endtime set n=NULL.")
  expect_message(pp_simulate(hp_obj,start = 2, end = 3, verbose = TRUE),
                 "Simulating up to endtime. To simulate n events specify n.")
  expect_null(pp_simulate(hp_obj, start = 2, end = 2))
  expect_null(pp_simulate(hp_obj, start = 2, end = 1))
  expect_error(pp_simulate(hp_obj, start = 2, end = 10, n = 0),
               "n must be positive for simulation.")
  expect_error(pp_simulate(hp_obj, start = 2, end = 10, n = -2),
               "n must be positive for simulation.")
  
  ## tests for mmhp
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  x <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3), lambda0 = 0.9,
            lambda1 = 1.1, alpha = 0.8, beta = 1.2)
  expect_type(x, "list")
  expect_gte(length(x), 7)
  
  #test for error messages for mmhp
  expect_error(pp_simulate(pp_mmhp(lambda0 = 0.9, lambda1 = 1.1,
                                 alpha = 1, beta = 0.5)),
               "Require alpha less than beta for a stationary process")
  expect_error(pp_simulate(pp_mmhp(lambda0 = 0.9, lambda1 = 1.1,
                                alpha = 0.1, beta = 0.5)),
               "Invalid delta")
  expect_error(pp_simulate(pp_mmhp(lambda0 = 0.9, lambda1 = 1.1,
                                alpha = 0.1, beta = 0.5, delta = c(0.5, 0.5))),
               "No Q matrix specified")
  expect_message(pp_simulate(pp_mmhp(lambda0 = 0.9, lambda1 = 1.1,
                                alpha = 0.1, beta = 0.5, delta = c(0.5, 0.5),
                                Q = matrix(c(-0.4, 0.4, 0.2, -0.2),
                                           ncol = 2, byrow = TRUE),
                                events = c(1,2,3)), verbose = TRUE),
               "Events in the mmhp object will be overwritten by simulated events.")
  
  
  ## other tests
  y <- pp_hpp(lambda = 1)
  class(y) <- "non_pp"
  expect_output(pp_simulate(y),
               "Please input the right model. Select from hp, hpp and mmhp.")
  
})
