test_that("test ksplot", {
  hpp_obj <- pp_hpp(lambda = 1)
  events <- pp_simulate(hpp_obj, end=10)
  # r <- pp_compensator(hpp_obj, events)
  expect_error(pp_ksplot(r = NULL),"No rescaled interevent times provided")
  expect_error(pp_ksplot(r = c(0)),"No rescaled interevent times provided")
  expect_error(pp_ksplot(r = c(-1,0,1)),"Incorrect interevent times provided")
})