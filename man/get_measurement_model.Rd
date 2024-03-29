% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{get_measurement_model}
\alias{get_measurement_model}
\title{Generate lavaan syntax for measurement model}
\usage{
get_measurement_model(factors_list)
}
\arguments{
\item{factors_list}{Named list: each element is a vector of items}
}
\value{
Character value to be used with \pkg{lavaan} as model syntax
}
\description{
This function generates the \pkg{lavaan} syntax for a measurement model. The
factor names and items are taken from a named list. This can be used by
itself (e.g. for a CFA) or in conjunction with a structural model (e.g. with
a mediation model).
}
\examples{
library(lavaan)
factors <- list(x="x"\%p\%1:3, y="y"\%p\%1:3)
factors |>
   get_measurement_model() |>
   sem(data=df) |>
   summary()

}
\concept{models}
