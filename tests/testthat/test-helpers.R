test_that("stop_if_not_scalar works with scalar", {

  x = 1
  expect_null(stop_if_not_scalar(x))

})

test_that("stop_if_not_scalar fails with vector", {

  x = 1:10
  expect_error(stop_if_not_scalar(x),
               regexp = "Argument `x` is a parameter that has to be a scalar but is of length 10.")

})
