% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataprep.R
\name{add_composites}
\alias{add_composites}
\title{Add composites (item means)}
\usage{
add_composites(
  df,
  map,
  fn = rowMeans,
  na.rm = TRUE,
  standardize_items = FALSE,
  check_already_exist = FALSE,
  ...
)
}
\arguments{
\item{df}{data.frame}

\item{map}{named list where names are new variable names, elements are
character vectors with variable names to use}

\item{fn}{function to apply to each row (default \code{\link[base:colSums]{base::rowMeans()}})}

\item{na.rm}{(logical) remove missing (NA) before computing composite?
(default \code{TRUE})}

\item{standardize_items}{(logical) standardize items before computing composite?
(default \code{FALSE})}

\item{check_already_exist}{(logical) stop if variable already exists?
(default \code{FALSE})}

\item{...}{other arguments passed to \code{fn()}}
}
\value{
\code{df} with added composite variables
}
\description{
This function adds composites (item means by default). The Cronbach's
alpha is also printed to the console.
}
\details{
Function in \code{fn} must take \code{na.rm} as an argument, and return a vector with
a value for each row in \code{df}. Good candidate functions are \code{\link[base:colSums]{base::rowMeans()}}
and \code{\link[base:colSums]{base::rowSums()}}. Users can also supply custom functions.
}
\examples{
comp <- list(X=c("x1","x2","x3"))
df <- df |> add_composites(comp)

}
\concept{data_prep}
