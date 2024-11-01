% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling.R
\name{count_sorted}
\alias{count_sorted}
\title{Count a variable or variables sorted}
\usage{
count_sorted(df, ...)
}
\arguments{
\item{df}{a data.frame}

\item{...}{the variables to use and other arguments to count}
}
\description{
It returns the ordered counts of the variable in the data.frame.
}
\examples{
data.frame(a = c("x", "y", "x"), b = c("z", "z", "n")) |>
  count_sorted(a)
}
\keyword{wrangling}
