test_that("test hawkes_max_intensity", {
  
  ## alpha = 0
  hp_obj <- pp_hp(lambda = 1, alpha = 0, beta = 2)
  events <- c(1,2,3)
  expect_equal(hawkes_max_intensity(hp_obj, events), 1)
  
  ## only 1 event
  hp_obj <- pp_hp(lambda = 1, alpha = 2, beta = 2.5)
  events <- c(1)
  expect_equal(hawkes_max_intensity(hp_obj, events), 3)
  
  expect_error(hawkes_max_intensity(pp_hp(lambda = 1, alpha = 2.5, beta = 2.5),
                                    events),
               "A stationary Hawkes process requires alpha<beta.")
  expect_error(hawkes_max_intensity(pp_hp(lambda = 1, alpha = 3.5, beta = 2.5),
                                    events),
               "A stationary Hawkes process requires alpha<beta.")
  expect_error(hawkes_max_intensity(pp_hp(lambda = 1, alpha = 3.5, beta = 2.5),
                                    events))
})
