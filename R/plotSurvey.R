#' @title Plot Survey
#' @name plotSurvey
#'
#' @param data A data frame containing survey data.
#' @param question A character string specifying the column name of the data to visualize.
#' @param type A character string indicating the type of visualization: "bar", "pie", or "histogram".
#'
#' @return A plot object displaying the chosen visualization.
#' @export
#'
#' @examples data <- data.frame(satisfaction = c("Satisfied", "Dissatisfied", "Satisfied", "Neutral"))
#'           plotSurvey(data, question = "satisfaction", type = "bar")

library(ggplot2)

plotSurvey <- function(data, question, type) {
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

  # Generate the specified type of plot
  if (type == "bar") {
    p <- ggplot(data, aes(x = !!sym(question))) +
      geom_bar() +
      xlab(question) +
      ylab("Count") +
      ggtitle(paste("Bar Chart of", question))
  } else if (type == "pie") {
    p <- ggplot(data, aes(x = "", fill = !!sym(question))) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      xlab("") +
      ylab("") +
      ggtitle(paste("Pie Chart of", question)) +
      theme_void()
  } else if (type == "histogram") {
    p <- ggplot(data, aes(x = !!sym(question))) +
      geom_histogram(binwidth = 1) +
      xlab(question) +
      ylab("Frequency") +
      ggtitle(paste("Histogram of", question))
  } else {
    stop("Unsupported plot type. Use 'bar', 'pie', or 'histogram'.")
  }

  return(p)
}
