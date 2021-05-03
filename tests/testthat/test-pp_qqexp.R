test_that("test pp_qqexp simple cases", {
  expect_error(
    pp_qqexp(r = NULL),
    "No rescaled interevent times provided"
  )
  expect_error(
    pp_qqexp(r = 0),
    "No rescaled interevent times provided"
  )
  expect_error(
    pp_qqexp(r = c(1, -1)),
    "Incorrect interevent times provided"
  )
})
