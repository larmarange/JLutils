% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_model.R
\name{tidy_model}
\alias{tidy_model}
\title{A generic tidier for model outputs}
\usage{
tidy_model(
  x,
  conf.int = FALSE,
  conf.level = 0.95,
  exponentiate = FALSE,
  quick = FALSE,
  ...
)
}
\arguments{
\item{x}{a model object}

\item{conf.int}{whether to include a confidence interval}

\item{conf.level}{confidence level of the interval, used only if \code{conf.int=TRUE}}

\item{exponentiate}{whether to exponentiate the coefficient estimates and confidence intervals}

\item{quick}{whether to compute a smaller and faster version, containing only the term and estimate columns.}

\item{...}{extra arguments}
}
\description{
\lifecycle{experimental}
}
