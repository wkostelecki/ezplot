% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendar_plot.R
\name{calendar_plot}
\alias{calendar_plot}
\title{calendar_plot}
\usage{
calendar_plot(data, x, y, ...)
}
\arguments{
\item{data}{A data.frame.}

\item{x}{date column}

\item{y}{A named character value. Evaluates to a column.}

\item{...}{additional arguments for tile_plot}
}
\description{
calendar_plot
}
\examples{
library(tsibbledata)
calendar_plot(vic_elec, "Time", "Demand", zlim = c(NA, NA))
}
