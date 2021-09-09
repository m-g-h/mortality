test_that("survival_function_GM works with scalar age", {
  expect_equal(survival_function_GM(A = 0.00033,
                                     R = 0.00014,
                                     alpha = 0.11521,
                                     t = 0),
               1)

})

test_that("survival_function_GM works with vector age", {
  expect_equal(survival_function_GM(A = 0.00033,
                                     R = 0.00014,
                                     alpha = 0.11521,
                                     t = 0:100),
               readRDS(file = "test-survival_function_GM_vector"))

})

test_that("survival_function_GM fails for vector parameters",{

  expect_error(survival_function_GM(A = 1:10,
                                     R = 1,
                                     alpha = 1,
                                     t = 1),
               regexp = "Argument `A` is a parameter that has to be a scalar but is of length 10."
  )

  expect_error(survival_function_GM(A = 1,
                                     R = 1:10,
                                     alpha = 1,
                                     t = 1),
               regexp = "Argument `R` is a parameter that has to be a scalar but is of length 10."
  )

  expect_error(survival_function_GM(A = 1,
                                     R = 1,
                                     alpha = 01:10,
                                     t = 1),
               regexp = "Argument `alpha` is a parameter that has to be a scalar but is of length 10."
  )

})


