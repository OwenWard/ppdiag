test_that("test drawHPPIntensity", {
  
  hpp_obj <- pp_hpp(lambda = 1)
  expect_error(drawHPPIntensity(hpp_obj, events = NULL),"Events must be provided either in the object or in the events argument. ")
  
  events <- pp_simulate(hpp_obj, end = 10)
  expect_message(drawHPPIntensity(hpp_obj, events = events),"Using the hpp object. Set fit=TRUE to fit events provided. ")
  expect_message(drawHPPIntensity(hpp_obj, events = events, fit = TRUE),"Fitting provided events.")
  
  hpp_obj$events <- events
  expect_message(drawHPPIntensity(hpp_obj, events = NULL, fit = TRUE),"No events provided. Using the hpp object.")
  expect_message(drawHPPIntensity(hpp_obj, events = events),"Using the hpp object. Set fit=TRUE to fit events provided. ")
  
  #test if only events is provided
  expect_error(drawHPPIntensity(events = events),"No object provided, set fit=TRUE to fit the events provided.")
  expect_message(drawHPPIntensity(events = events, fit = TRUE),"Fitting provided events.")
})