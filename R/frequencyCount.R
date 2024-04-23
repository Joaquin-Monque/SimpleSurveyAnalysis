#' @title Frequency Count
#' @name frequencyCount
#'
#' @param data A data frame containing survey data.
#' @param question A character string specifying the column name of the categorical question to analyze.
#'
#' @return A data frame showing each response option, its frequency count, and percentage of total responses.
#' @export
#'
#' @examples data <- data.frame(satisfaction = c("Satisfied", "Dissatisfied", "Satisfied", "Neutral"))
#'           frequencyCount(data, question = "satisfaction")

library(dplyr)
library(tidyr)

frequencyCount <- function(data, question) {
  # Ensure the data input is a dataframe and question is a character string
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe")
  }
  if (!is.character(question) || length(question) != 1) {
    stop("Input 'question' must be a single column name as a character string")
  }

  # Check if the specified question exists in the dataframe
  if (!(question %in% names(data))) {
    stop("Specified column does not exist in the data frame")
  }

  # Calculate frequency and percentage for the categorical question
  data %>%
    count(!!sym(question), name = "Frequency") %>%
    mutate(Percentage = Frequency / sum(Frequency) * 100)
}

