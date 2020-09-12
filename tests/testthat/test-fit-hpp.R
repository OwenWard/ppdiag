test_that("fit hpp", {
  pois_y <- hpp(lambda = 1, end = 10)
  events <- simulatehpp(pois_y)
  hpp=fithpp(events)
  expect_type(hpp, "list")
  expect_length(hpp, 5)
})