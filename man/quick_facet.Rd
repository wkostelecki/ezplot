% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{quick_facet}
\alias{quick_facet}
\title{Quick facet}
\usage{
quick_facet(g, ncol = NULL, ...)
}
\arguments{
\item{g}{A ggplot object.}

\item{ncol}{Number of facet columns.}

\item{...}{Arguments to pass to \code{facet_grid} or \code{facet_wrap}.}
}
\description{
Applies faceting to ggplot objects when g[["data"]] has a
  \code{facet_x} or \code{facet_y} column.
}
