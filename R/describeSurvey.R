#' @title Describe Survey
#' @name describeSurvey
#'
#' @param data A data frame containing survey data.
#' @param variables A character vector of column names for which descriptive statistics are calculated.
#'
#' @return A data frame with one row per variable and columns for each statistic.
#' @export
#'
#' @examples
#' data <- data.frame(age = c(25, 30, 35), income = c(50000, 60000, 70000))
#' describeSurvey(data = survey_data, variables = c("age","income"))

library(dplyr)

describeSurvey <- function(data, variables) {
  # Ensure the data input is a dataframe and variables is a character vector
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe")
  }
  if (!is.character(variables)) {
    stop("Input 'variables' must be a character vector of column names")
  }

  # Select the specified variables and calculate the descriptive statistics
  results <- data %>%
    select(all_of(variables)) %>%
    summarise(across(.cols = everything(),
                     .fns = list(
                       mean = ~mean(.x, na.rm = TRUE),
                       median = ~median(.x, na.rm = TRUE),
                       mode = ~Mode(.x),
                       min = ~min(.x, na.rm = TRUE),
                       max = ~max(.x, na.rm = TRUE),
                       range = ~paste(min(.x, na.rm = TRUE), max(.x, na.rm = TRUE), sep = "-"),
                       variance = ~var(.x, na.rm = TRUE),
                       sd = ~sd(.x, na.rm = TRUE)
                     ),
                     .names = "{.col}_{.fn}"))

  return(results)
}

# Helper function to calculate mode since R does not have a built-in mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
