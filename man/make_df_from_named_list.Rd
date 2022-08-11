% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{make_df_from_named_list}
\alias{make_df_from_named_list}
\title{Combine rows of named list of 2-column data.frames}
\usage{
make_df_from_named_list(
  list,
  index = "Var",
  value = paste0("Value", 1:length(list[[1]]))
)
}
\arguments{
\item{list}{Named list of data.frames}

\item{index}{Variable name to store data.frame names}

\item{value}{Variable name for value stored in each data.frame (in ana_fn functions, typically a p-value)}
}
\value{
data.frame with combined rows and 2 columns
}
\description{
This function is an internal, helper function used by \code{ana_fn} functions
to combine p-values across several variables.
}
\seealso{
\code{\link[=ana_fn_aov]{ana_fn_aov()}}, \code{\link[=ana_fn_rm_aov]{ana_fn_rm_aov()}}, \code{\link[=ana_fn_chisq]{ana_fn_chisq()}}
}