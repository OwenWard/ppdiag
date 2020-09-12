test_that("simulate hp", {
  sim=simulatehp(hp=hp(1.5,0.1,0.2),end=10)
  expect_type(sim, "list")
  expect_equal(length(sim), 2)
  expect_true(!is.null(sim$t))
  
})
