test_that("test hp", {
  #test simulate hp
  sim=simulatehp(hp=hp(1.5,0.1,0.2),end=10)
  expect_type(sim, "list")
  expect_equal(length(sim), 2)
  expect_true(!is.null(sim$t))
  
  #test fit hp
  events=sim$t
  hp=fithp(vec=rep(0.1,3),t=events, end=10)
  expect_type(hp, "list")
  expect_equal(length(hp), 4)
  expect_lt(hp$alpha,hp$beta)
})
