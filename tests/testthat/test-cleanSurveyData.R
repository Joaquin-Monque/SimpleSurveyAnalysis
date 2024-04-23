test_that("cleanSurveyData handles missing values", {
  test_data <- data.frame(age = c(25, NA, 35), satisfaction = c("Good", NA, "Poor"))
  options <- list(missing = "fill", fill_value = 30)
  cleaned_data <- cleanSurveyData(test_data, options)
  expect_equal(cleaned_data$age[2], 30)
})
