% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidymodels.R
\name{score_recipe}
\alias{score_recipe}
\title{Tune a recipe using glmnet and lightgbm and stacks}
\usage{
score_recipe(rec, resamples, grids = list(10, 10), metric = "accuracy")
}
\arguments{
\item{rec}{recipe}

\item{resamples}{rset}

\item{grids}{for glmnet and lightgbm tuning}

\item{metric}{to be compared}
}
\description{
Tune a recipe using glmnet and lightgbm and stacks
}
\keyword{tidymodels}
