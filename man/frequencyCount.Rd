% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequencyCount.R
\name{frequencyCount}
\alias{frequencyCount}
\title{Frequency Count}
\arguments{
\item{data}{A data frame containing survey data.}

\item{question}{A character string specifying the column name of the categorical question to analyze.}
}
\value{
A data frame showing each response option, its frequency count, and percentage of total responses.
}
\description{
Frequency Count
}
\examples{
data <- data.frame(satisfaction = c("Satisfied", "Dissatisfied", "Satisfied", "Neutral"))
          frequencyCount(data, question = "satisfaction")
}
