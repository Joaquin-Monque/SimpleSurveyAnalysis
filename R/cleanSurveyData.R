#' @title Clean Survey Data
#' @name cleanSurveyData
#'
#' @param data A data frame containing survey data.
#' @param options A list of options specifying how to handle missing data and recoding of responses.
#'
#' @return A data frame with cleaned and preprocessed data.
#' @export
#'
#' @examples data <- data.frame(age = c(25, NA, 35), satisfaction = c("Satisfied", "Satisfied", NA))
#'           options <- list(missing = "fill", fill_value = "No Response", recode = TRUE,
#'                 recode_rules = list(satisfaction = c("Not Satisfied" = "Dissatisfied")))
#' cleanSurveyData(data, options)

library(dplyr)
library(forcats)

cleanSurveyData <- function(data, options) {
  # Ensure the data input is a dataframe
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe")
  }

  # Check options
  if (!is.list(options)) {
    stop("Input 'options' must be a list containing data handling preferences")
  }

  # Handling missing data based on options
  if (!is.null(options$missing)) {
    if (options$missing == "exclude") {
      data <- data %>% na.omit()
    } else if (options$missing == "fill") {
      data <- data %>% mutate(across(everything(), ~ifelse(is.na(.), options$fill_value, .)))
    }
  }

  # Recoding response categories based on options
  if (!is.null(options$recode) && options$recode) {
    # This example assumes 'options' contains 'recode_rules', a list of vectors specifying recoding
    for (column in names(options$recode_rules)) {
      data[[column]] <- fct_recode(as.factor(data[[column]]), !!!options$recode_rules[[column]])
    }
  }

  return(data)
}
