test_that("Correct output length based on number of events", {
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  mmpp_obj <- pp_mmpp(Q, delta = c(1 / 3, 2 / 3),
                      lambda0 = 0.9, c = 1.1)
  expect_error(mmpp_event_state(params = mmpp_obj, events = c()),
               "No events provided")
  curr_events <- c(1,2,3,4,5)
  output <- mmpp_event_state(params = mmpp_obj, events = curr_events)
  expect_length(output$pzt, length(curr_events) )
  expect_length(output$zt, length(curr_events) )
  
})