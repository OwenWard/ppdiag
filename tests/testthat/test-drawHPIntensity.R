test_that("test drawHPIntensity", {
  
  hp_obj <- pp_hp(lambda0 = 0.1, alpha = 0.45, beta = 0.5)
  expect_error(drawHPIntensity(hp_obj, events = NULL),"Events must be provided either in the object or in the events argument.")
  
  sims <- pp_simulate(hp_obj, n = 20 )
  events <- sims
  expect_message(drawHPIntensity(hp_obj, events = events, verbose = TRUE),
                 "Using the hp object. Set fit = TRUE to fit events provided.")
  
  sims <- pp_simulate(hp_obj, n = 10)
  events <- sims
  expect_message(drawHPIntensity(hp_obj, events = events,
                                 fit = TRUE, verbose = TRUE),
                 "Fitting provided events.")
  
  hp_obj$events <- events
  expect_message(drawHPIntensity(hp_obj, events = NULL, 
                                 fit = TRUE, verbose = TRUE),
                 "No events provided. Using the hp object.")
  expect_message(drawHPIntensity(hp_obj, events = events, verbose = TRUE),
                 "Using the hp object. Set fit=TRUE to fit events provided.")

  #test if only events is provided
  expect_error(drawHPIntensity(events = events),"No object provided, set fit=TRUE to fit the events provided.")
  expect_message(drawHPIntensity(events = events, fit = TRUE, verbose = TRUE),
                 "Fitting provided events.")
})