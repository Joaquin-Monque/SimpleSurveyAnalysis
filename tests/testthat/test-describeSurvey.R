test_that("describeSurvey returns correct statistics", {
  test_data <- data.frame(age = c(21, 22, 23), income = c(100, 200, 300))
  results <- describeSurvey(test_data, c("age", "income"))
  expect_true("age_mean" %in% names(results))
  expect_equal(results$age_mean, mean(c(21, 22, 23)))
})
