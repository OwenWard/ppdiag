test_that("hpp_step tests", {
  expect_error(plothpp_step(events = NULL),
               "No events provided")
  expect_error(plothpp_step(events = 0),
               "No events provided")
})
