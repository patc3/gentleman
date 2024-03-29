% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataprep.R
\name{make_factors_into_effect_codes}
\alias{make_factors_into_effect_codes}
\title{Generates effect codes for factors}
\usage{
make_factors_into_effect_codes(
  df,
  factors,
  remove_selected_columns = TRUE,
  ignore_na = TRUE,
  change_which_dummy_is_removed = FALSE,
  make_into_dummy_instead = c()
)
}
\arguments{
\item{df}{A data.frame}

\item{factors}{Vector of factor names found in \code{df}}

\item{remove_selected_columns}{(logical) Whether to remove original factors in \code{factors} (default \code{TRUE})}

\item{ignore_na}{(logical) Whether to ignore NA. If \code{FALSE}, NA will be treated as a factor level (default \code{TRUE})}

\item{change_which_dummy_is_removed}{(logical) Randomize factor levels prior to creating codes (default \code{FALSE})}

\item{make_into_dummy_instead}{Vector of variables in \code{factors} for which to obtain dummy codes instead.}
}
\value{
\code{df} with effect codes added (and original factors possibly removed)
}
\description{
This function creates new variables representing effect codes for selected
factors. It can also be used to generate dummy codes instead.
}
\details{
This function uses the library \code{fastDummies} to generate dummy codes,
then recodes the dummy codes into effect codes (except for variables whose name is
also specified in \code{make_into_dummy_instead}).
}
\examples{
\dontrun{
df |> make_factors_into_effect_codes(
  factors=c("Gender", "Nationality"),
  make_into_dummy_instead = "Gender"
)
}

}
\concept{data_prep}
