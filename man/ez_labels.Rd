% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ez_labels.R
\name{ez_labels}
\alias{ez_labels}
\title{Function for formatting numeric labels}
\usage{
ez_labels(
  x,
  prepend = "",
  append = "",
  as_factor = FALSE,
  round = Inf,
  signif = Inf,
  sign = FALSE
)
}
\arguments{
\item{x}{numeric}

\item{prepend}{character}

\item{append}{character}

\item{as_factor}{logical}

\item{round}{numeric passed to \code{round()}}

\item{signif}{numeric passed to \code{signif()}}

\item{sign}{logical. Adds a plus sign to positive numbers}
}
\value{
y
}
\description{
Function for formatting numeric labels
}
\examples{
ez_labels(10^(0:10))
ez_labels(2000, append = " apples")
ez_labels(0:10, append = " apples", as_factor = TRUE)
ez_labels(c(0, 0.1, 0.01, 0.001, 0.0001))
ez_labels(c(-1, -0.0001, 0, 0.0001, 1, NA), round = 2, sign = FALSE)
ez_labels(c(-1, -0.0001, 0, 0.0001, 1, NA), round = 2, sign = TRUE)
}
