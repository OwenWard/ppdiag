test_that("test hp", {
  #test simulate hp
  sim <- simulatehp(hp = pp_hp(1.5,0.1,0.2), end = 10)
  expect_type(sim, "list")
  expect_equal(length(sim), 2)
  expect_true(!is.null(sim$events))
  
  
  #test for edge cases
  #start>end
  hp_obj <- pp_hp(1.5,0.1,0.2)
  sim <- simulatehp(hp_obj, start=20, end = 10)
  #start==end
  expect_null(sim)
  sim <- simulatehp(hp_obj, start=20, end = 20)
  expect_null(sim)
  
  # test alpha >= beta
  expect_error(simulatehp(pp_hp(1.5,1.2,1), end = 10),
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
  init <- rep(0.3,3)
  hp_obj <- pp_hp(lambda0 = 0.1, alpha = 0.45, beta = 0.5)
  sims <- pp_simulate(hp_obj, start = 0, n = 10)
  expect_type(fithp(init, sims$events), "list")
  expect_length(fithp(init, sims$events), 4)
  expect_lt(fithp(init, sims$events)$alpha,fithp(init, sims$events)$beta)
  
  expect_type(fithp(rep(3,3), sims$events), "list")
  expect_length(fithp(init, sims$events), 4)
  expect_lt(fithp(init, sims$events)$alpha,fithp(init, sims$events)$beta)
  
})
