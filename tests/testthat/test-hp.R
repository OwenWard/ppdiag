test_that("test hp", {
  #test simulate hp
  sim=simulatehp(hp=hp(1.5,0.1,0.2),end=10)
  expect_type(sim, "list")
  expect_equal(length(sim), 2)
  expect_true(!is.null(sim$t))
  
  #test for error messages
  expect_error(simulatehp(hp=hp(1.5,0.5,0.2),end=10), "A stationary hawkes process requires alpha<beta.")
  expect_error(simulatehp(hp=hp(1.5,0.1,0.2,events=c(1,5,10)),end=10), "Event time already in the hp object.")
  
  #test fit hp
  events=sim$t
  hp=fithp(vec=rep(0.1,3),t=events, end=10)
  expect_error(fithp(vec=rep(0.1,3),t=events, end=10),"Refitting exceeded 10 times. Try a different initial vector. ")
  #expect_type(hp, "list")
  #expect_equal(length(hp), 4)
  #expect_lt(hp$alpha,hp$beta)
  
  
  hp=fithp(vec=rep(0.5,3),t=events, end=10)
  expect_error(fithp(vec=rep(0.5,3),t=events, end=10),"Refitting exceeded 10 times. Try a different initial vector. ")
   #expect_type(hp, "list")
   #expect_equal(length(hp), 4)
   #expect_lt(hp$alpha,hp$beta)
  
  
  hp=fithp(vec=rep(0.3,3),t=events, end=10)
  expect_error(fithp(vec=rep(0.3,3),t=events, end=10),"Refitting exceeded 10 times. Try a different initial vector. ")
   #expect_type(hp, "list")
   #expect_equal(length(hp), 4)
   #expect_lt(hp$alpha,hp$beta)
})
