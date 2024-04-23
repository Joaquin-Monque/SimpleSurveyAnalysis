test_that("frequencyCount calculates frequencies correctly", {
  test_data <- data.frame(response = c("Yes", "No", "Yes"))
  results <- frequencyCount(test_data, "response")
  expect_equal(nrow(results), 2)
  expect_equal(results$Frequency[results$response == "Yes"], 2)
})
