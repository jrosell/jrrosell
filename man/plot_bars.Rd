% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling.R
\name{plot_bars}
\alias{plot_bars}
\title{Plot bars for non double columns}
\usage{
plot_bars(df, ..., top_values = 50)
}
\arguments{
\item{df}{a data.frame}

\item{...}{optional parameters to geom_histogram}

\item{top_values}{fist most common values (default 50)}
}
\description{
Plot bars for non double columns
}
\examples{
plot_bars(data.frame(a = c("x", "y"), b = c("z", "z")))
}
\keyword{wrangling}
