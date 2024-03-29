% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataprep.R
\name{cast}
\alias{cast}
\title{Cast variables of one type to another}
\usage{
cast(df, type_from, type_to, vars = NULL)
}
\arguments{
\item{df}{data.frame}

\item{type_from}{original type (a class from \code{\link[base:class]{base::inherits()}})}

\item{type_to}{new type (from calling \code{as.*})}

\item{vars}{(character) vector of variable names to change
if they are of type \code{type_from} (if \code{NULL}, use all)}
}
\value{
data.frame with types changed
}
\description{
Cast variables of one type to another
}
\examples{
\dontrun{
df |>
   cast("numeric", "factor") |>
   get_desc_table(c("Age", "Nationality"), tbl_fn=tbl_fn_fac)
}

}
\concept{data_prep}
