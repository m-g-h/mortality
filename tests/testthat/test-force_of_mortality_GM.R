test_that("force_of_mortality_GM works with scalar age", {
  expect_equal(force_of_mortality_GM(A = 0.00033,
                                     R = 0.00014,
                                     alpha = 0.11521,
                                     t = 0),
               0.00046999999999999999)

})

test_that("force_of_mortality_GM works with vector age", {
  expect_equal(force_of_mortality_GM(A = 0.00033,
                                     R = 0.00014,
                                     alpha = 0.11521,
                                     t = 0:100),
               readRDS(file = "test-force_of_mortality_GM_vector"))

})

test_that("force_of_mortality_GM fails for vector parameters",{

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


