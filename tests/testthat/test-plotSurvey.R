test_that("plotSurvey creates a plot", {
  test_data <- data.frame(response = c("Yes", "No", "Yes"))
  plot <- plotSurvey(test_data, "response", "bar")
  expect_s3_class(plot, "ggplot")
})
