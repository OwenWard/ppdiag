test_that("test pp_ksplot", {
  expect_error(
    pp_ksplot(r = NULL),
    "No rescaled interevent times provided"
  )
  expect_error(
    pp_ksplot(r = 0),
    "No rescaled interevent times provided"
  )
  expect_error(
    pp_ksplot(r = c(1, -1)),
    "Incorrect interevent times provided"
  )
})
