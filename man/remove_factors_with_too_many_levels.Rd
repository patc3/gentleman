% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataprep.R
\name{remove_factors_with_too_many_levels}
\alias{remove_factors_with_too_many_levels}
\title{Remove factors with too many levels}
\usage{
remove_factors_with_too_many_levels(df, maxlevels = 20)
}
\arguments{
\item{df}{data.frame}

\item{maxlevels}{(integer) maximum number of levels for factors}
}
\value{
\code{df} with factors with more than \code{maxlevels} levels removed
}
\description{
This function removes from a data.frame factors that have
too many levels.
}
\examples{
df <- df |> remove_factors_with_too_many_levels()

}
\concept{data_prep}
