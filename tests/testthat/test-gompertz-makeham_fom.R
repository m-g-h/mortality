test_that("GM_force_of_mortality works with scalar age", {
  expect_equal(force_of_mortality_GM(A = 0.005,
                                     R = 0.00005,
                                     alpha = 0.0093,
                                     t = 10),
               0.0050548730867634043)

})

test_that("GM_force_of_mortality works with vector age", {
  expect_equal(force_of_mortality_GM(A = 0.005,
                                     R = 0.00005,
                                     alpha = 0.0093,
                                     t = 0:100),
               readRDS(file = "test-gompertz-makeham_GM_vector"))

})

test_that("GM_force_of_mortality fails for vector parameters",{

  expect_error(force_of_mortality_GM(A = 1:10,
                                     R = 1,
                                     alpha = 1,
                                     t = 1),
               regexp = "Argument `A` is a parameter that has to be a scalar but is of length 10."
  )

  expect_error(force_of_mortality_GM(A = 1,
                                     R = 1:10,
                                     alpha = 1,
                                     t = 1),
               regexp = "Argument `R` is a parameter that has to be a scalar but is of length 10."
  )

  expect_error(force_of_mortality_GM(A = 1,
                                     R = 1,
                                     alpha = 01:10,
                                     t = 1),
               regexp = "Argument `alpha` is a parameter that has to be a scalar but is of length 10."
  )

})


