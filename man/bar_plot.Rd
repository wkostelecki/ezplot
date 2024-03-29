% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bar_plot.R
\name{bar_plot}
\alias{bar_plot}
\title{bar_plot}
\usage{
bar_plot(
  data,
  x,
  y = "1",
  group = NULL,
  facet_x = NULL,
  facet_y = NULL,
  size = 11,
  width = NULL,
  reorder = c("group", "facet_x", "facet_y"),
  palette = ez_col,
  labels_y = if (position == "fill") {
     function(x) ez_labels(100 * x, append =
    "\%")
 } else {
     ez_labels
 },
  labels_x = identity,
  label_pos = c("auto", "inside", "top", "both", "none"),
  label_inside = c("y", "absolute", "share", "percent", "both"),
  rescale_y = 1.1,
  label_cutoff = 0.12,
  use_theme = theme_ez,
  position = "stack",
  facet_scales = "fixed",
  legend_ncol = NULL,
  coord_flip = FALSE,
  angle = 0,
  repel = FALSE
)
}
\arguments{
\item{data}{A data.frame.}

\item{x}{A named character value. Evaluates to a column.}

\item{y}{A named character value. Evaluates to a column.}

\item{group}{A character value. Evaluates to a column.}

\item{facet_x}{A character value. Evaluates to a column.}

\item{facet_y}{A character. Evaluates to a column.}

\item{size}{theme size for \code{use_theme()}. Default is 14.}

\item{width}{Width of bar.}

\item{reorder}{A character vector specifying the group variables to reorder.
Default is \code{c("group", "facet_x", "facet_y")}.}

\item{palette}{Colour function.}

\item{labels_y}{label formatting function}

\item{labels_x}{label formatting function}

\item{label_pos}{Position of labels. Can be "auto", "inside", "top", "both"
or "none".}

\item{label_inside}{Value to display inside bar segments. Options are "y", "absolute", "percent", "share" or "both".}

\item{rescale_y}{Rescaling factor for y-axis limits}

\item{label_cutoff}{Cutoff size (proportion of y data range) for excluding
labels}

\item{use_theme}{ggplot theme function}

\item{position}{Either \code{"stack"} (default), \code{"fill"} or \code{"dodge"}}

\item{facet_scales}{Option passed to scales argument in \code{facet_wrap} or
\code{facet_grid}. Default is \code{"fixed"}.}

\item{legend_ncol}{Number of columns in legend.}

\item{coord_flip}{logical (default is FALSE). If TRUE, flips the x and y
coordinate using ggplot2::coord_flip()}

\item{angle}{angle for geom_text(_repel)}

\item{repel}{logical (default if FALSE). If TRUE, uses ggrepel for geom_text}
}
\value{
A ggplot object.
}
\description{
bar_plot
}
\examples{
library(tsibble)
library(tsibbledata)
library(lubridate)

bar_plot(ansett, "year(Week)", "Passengers")
bar_plot(ansett, "year(Week)", "Passengers", "Class", label_pos = "both")
bar_plot(ansett, "year(Week)", "Passengers", "Class", label_pos = "both", label_inside = "both")
bar_plot(ansett, "year(Week)", "Passengers", "Class", coord_flip = TRUE)
}
