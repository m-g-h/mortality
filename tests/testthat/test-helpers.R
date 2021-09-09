# stop_if_not_scalar ------------------------------------------------------

test_that("stop_if_not_scalar works with scalar", {

  x = 1
  expect_null(stop_if_not_scalar(x))

})

test_that("stop_if_not_scalar fails with vector", {

  x = 1:10
  expect_error(stop_if_not_scalar(x),
               regexp = "Argument `x` is a parameter that has to be a scalar but is of length 10.")

})


# upper_inc_gamma ---------------------------------------------------------

test_that("upper_inc_gamma works", {

  expect_equal(upper_inc_gamma(-0.005, 2),
               0.048660336175908619)

})

test_that("upper_inc_gamma fails if inputs are not vectors",{
  # Check argment 1
  expect_error(upper_inc_gamma(1:2, 2),
               regexp = "Argument `u` is a parameter that has to be a scalar but is of length 2.")

  # Check argument 2
  expect_error(upper_inc_gamma(1, 1:2),
               regexp = "Argument `x` is a parameter that has to be a scalar but is of length 2.")
})
