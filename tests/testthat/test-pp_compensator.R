test_that("test pp_compensator messages", {
  obj <- pp_hpp(lambda = 1)
  class(obj) <- "non-pp"
  expect_output(
    pp_compensator(object = obj, events = c(1, 2)),
    "Please input the right model. Select from hp, hpp and mmhp."
  )
})

test_that("simple cases correct", {
  expect_identical(pp_compensator(object = pp_hpp(lambda = 1), events = 0), 0)
  expect_identical(pp_compensator(
    object = pp_hpp(lambda = 1),
    events = c(0, 1)
  ), 1)

  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  obj <- pp_mmhp(Q,
    delta = c(1 / 3, 2 / 3),
    lambda0 = 0.9,
    lambda1 = 1.1,
    alpha = 0.8, beta = 1.2
  )
  expect_identical(pp_compensator(object = obj, events = NULL), 0)

  expect_length(pp_compensator(object = obj, events = c(0, 1, 2)), 2)

  expect_identical(pp_compensator(
    object = pp_hp(
      lambda = 1,
      alpha = 0,
      beta = 1
    ),
    events = c(0, 1)
  ), c(1))
  expect_identical(pp_compensator(
    object = pp_hp(
      lambda = 1,
      alpha = 0,
      beta = 1
    ),
    events = c(0, 1, 2)
  ), c(1, 1))
  expect_identical(pp_compensator(
    object = pp_hp(
      lambda = 1,
      alpha = 0,
      beta = 1
    ),
    events = c(0)
  ), 0)

  Q <- matrix(c(-0.4, 0.4, 0.2, -0.2), ncol = 2, byrow = TRUE)
  obj <- pp_mmpp(Q,
    delta = c(1 / 3, 2 / 3),
    lambda0 = 1,
    c = 0
  )
  expect_identical(pp_compensator(object = obj, events = c(0, 1)), 1)
  expect_length(pp_compensator(object = obj, events = c(0, 1, 2)), 2)
})
