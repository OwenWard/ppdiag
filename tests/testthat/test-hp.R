test_that("test hp", {
  #test simulate hp
  sim <- simulatehp(hp = hp(1.5,0.1,0.2), end = 10)
  expect_type(sim, "list")
  expect_equal(length(sim), 2)
  expect_true(!is.null(sim$events))
  
  
  #test for edge cases
  #start>end
  hp_obj <- hp(1.5,0.1,0.2)
  sim <- simulatehp(hp_obj, start=20, end = 10)
  #start==end
  expect_null(sim)
  sim <- simulatehp(hp_obj, start=20, end = 20)
  expect_null(sim)
  
  # test alpha >= beta
  expect_error(simulatehp(hp(1.5,1.2,1), end = 10),
               "A stationary Hawkes process requires alpha<beta.")
  
  #n=0
  expect_error(simulatehp(hp_obj, start = 2, end = 10, n = 0),
               "n must be positive for simulation.")
  
  #test for messages
  expect_message(simulatehp(hp_obj,start=2,end=3,n=10),
                 "10 events simulated. To simulate up to endtime set n=NULL.")
  expect_message(simulatehp(hp_obj,start=2,end=3),
                 "Simulating up to endtime. To simulate n events specify n.")
  
  #test fit hp
  events <- sim$events
  #hp=fithp(vec=rep(0.1,3),t=events, end=10)
  #expect_error(fithp(vec=rep(0.1,3),t=events, end=10),
  # "Refitting exceeded 10 times. Try a different initial vector. ")
  #expect_type(hp, "list")
  #expect_equal(length(hp), 4)
  #expect_lt(hp$alpha,hp$beta)
  
  
  # hp <- fithp(vec=rep(0.5,3),t=events, end=10)
  #expect_error(fithp(vec=rep(0.5,3),t=events, end=10),
  # "Refitting exceeded 10 times. Try a different initial vector. ")
  #expect_type(hp, "list")
  #expect_equal(length(hp), 4)
  #expect_lt(hp$alpha,hp$beta)
  
  
  # hp <- fithp(vec=rep(0.3,3),t=events, end=10)
  #expect_error(fithp(vec=rep(0.3,3),t=events, end=10),
  # "Refitting exceeded 10 times. Try a different initial vector. ")
  #expect_type(hp, "list")
  #expect_equal(length(hp), 4)
  #expect_lt(hp$alpha,hp$beta)
})
