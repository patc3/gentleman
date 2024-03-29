% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml.R
\name{get_automl_config}
\alias{get_automl_config}
\title{Create configuration object for running AutoML}
\usage{
get_automl_config(
  include_algos = "DRF",
  max_runtime_secs = NULL,
  max_models = 1,
  max_runtime_secs_per_model = 60 * 5,
  balance_classes = FALSE
)
}
\arguments{
\item{include_algos}{(character) vector of included algorithms (see \code{\link[h2o:h2o.automl]{h2o::h2o.automl()}})}

\item{max_runtime_secs}{maximum time in seconds for H2O's AutoML to run (see \code{\link[h2o:h2o.automl]{h2o::h2o.automl()}})}

\item{max_models}{maximum number of models}

\item{max_runtime_secs_per_model}{maximum time in seconds for each model}

\item{balance_classes}{(logical) whether to balance classes for a categorical target (see \code{\link[h2o:h2o.automl]{h2o::h2o.automl()}})}
}
\value{
Configuration object (list) to pass to \code{\link[=get_automl_model]{get_automl_model()}} or \code{\link[=run_automl_pipeline]{run_automl_pipeline()}}
}
\description{
This function creates the configuration object
required to execute \code{\link[=get_automl_model]{get_automl_model()}} and
\code{\link[=run_automl_pipeline]{run_automl_pipeline()}}.
}
\examples{
\dontrun{
config <- get_automl_config()
pip <- df |> run_automl_pipeline(target="y1", config)
}

}
\seealso{
\code{\link[=get_automl_model]{get_automl_model()}}, \code{\link[=run_automl_pipeline]{run_automl_pipeline()}}, \code{\link[h2o:h2o.automl]{h2o::h2o.automl()}}
}
\concept{machine_learning}
