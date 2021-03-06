% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/performance_plot.R
\name{performance_plot}
\alias{performance_plot}
\title{performance_plot}
\usage{
performance_plot(
  data,
  fitted,
  actual,
  group = NULL,
  facet_x = NULL,
  facet_y = NULL,
  x = "fpr",
  y = "tpr",
  auc = c("title", "group"),
  size_line = 1,
  size = 11,
  env = parent.frame()
)
}
\arguments{
\item{data}{A data.frame.}

\item{fitted}{A character value. Evaluates to a numeric column.}

\item{actual}{A character value. Evaluates to a logical or binary column.}

\item{group}{A character value. Evaluates to a column.}

\item{facet_x}{A character value. Evaluates to a column.}

\item{facet_y}{A character. Evaluates to a column.}

\item{x}{ROCR::performance() measure}

\item{y}{ROCR::performance() measure}

\item{auc}{character vector indicating which AUC values should be displayed. Options are 'title' and 'group'}

\item{size_line}{width of line for \code{geom_line()}. Default is 1.}

\item{size}{theme size for \code{use_theme()}. Default is 14.}

\item{env}{environment for evaluating expressions.}
}
\description{
plots binary classification performance metrics
}
\examples{
performance_plot(mtcars, "-disp", "am")
performance_plot(mtcars, "-disp", "am", "cyl")
performance_plot(mtcars, "-disp", "am", "cyl", x = "rec", y = "prec")
performance_plot(mtcars, "-disp", "am", x = "rpp", y = "gain")
performance_plot(mtcars, "-disp", "am", x = "rpp", y = "lift")
performance_plot(mtcars, "-disp", "am", x = "cutoff", y = "tpr")
}
