test_that("handles null events", {
  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  mmhp_obj <- pp_mmhp(Q, delta = c(1 / 3, 2 / 3),
                      lambda0 = 0.9,lambda1 = 1.1,
                      alpha = 0.8, beta = 1.2)
  expect_error(mmhp_event_state(params = mmhp_obj, events = c()),
               "No events provided")
  curr_events <- c(1,2,3,4,5)
  output <- mmhp_event_state(params = mmhp_obj, events = curr_events)
  expect_length(output$pzt, length(curr_events) )
  expect_length(output$zt, length(curr_events) )
  
})
