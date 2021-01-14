test_that("test drawHPIntensity", {
  
  hp_obj <- pp_hp(lambda0 = 0.1, alpha = 0.45, beta = 0.5)
  expect_error(drawHPIntensity(hp_obj, events = NULL),"Events must be provided either in the object or in the events argument. ")
  
  sims <- pp_simulate(hp_obj, start = 0, end = 20)
  events <- sims$events
  expect_message(drawHPIntensity(hp_obj, events = events),"Using the hp object. Set fit = TRUE to fit events provided.")
  expect_error(drawHPIntensity(hp_obj, events = events, fit = TRUE),"Refitting exceeded 10 times. Try a different initial vector.")
  
  sims <- pp_simulate(hp_obj, start = 0, n = 10)
  events <- sims$events
  expect_message(drawHPIntensity(hp_obj, events = events, fit = TRUE),"Fitting provided events.")
  
  hp_obj$events <- events
  expect_message(drawHPIntensity(hp_obj, events = NULL, fit = TRUE),"No events provided. Using the hp object.")
  expect_message(drawHPIntensity(hp_obj, events = events),"Using the hp object. Set fit=TRUE to fit events provided. ")
})