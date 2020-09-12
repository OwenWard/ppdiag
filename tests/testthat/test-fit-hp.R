test_that("fit hp", {
  sim=simulatehp(hp=hp(1.5,0.1,0.2),end=10)
  events=sim$t
  hp=fithp(vec=rep(0.1,3),t=events, end=10)
  expect_type(hp, "list")
  expect_equal(length(hp), 4)
  expect_less_than(hp$alpha,hp$beta)
})
