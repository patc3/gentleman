#' Simulated cross-lagged dataset
#'
#' A simulated cross-lagged dataset with 2 measures (x and y) assessed
#' three times each (x1-x3, y1-y3). The cross-lagged model was generated by
#' \code{get_crosslagged_model()} and was fed to [simsem::generate()] with population
#' parameter values:
#' * \eqn{x_t \sim .7 \times x_{t-1}}
#' * \eqn{y_t \sim .7 \times x_{t-1}}
#' * \eqn{x_t \sim .4 \times y_{t-1}}
#' * \eqn{y_t \sim .4 \times y_{t-1}}
#' * \eqn{x_t \sim\sim .35 \times x_t}
#' * \eqn{y_t \sim\sim .35 \times y_t}
#' * \eqn{y_t \sim\sim .25 \times x_t}
#'
#'
#' @format A data frame with 500 rows and 6 variables:
#' \describe{
#'   \item{x1}{Numeric variable}
#'   \item{x2}{Numeric variable}
#'   \item{x3}{Numeric variable}
#'   \item{y1}{Numeric variable}
#'   \item{y2}{Numeric variable}
#'   \item{y3}{Numeric variable}
#' }
#' @source Simulated data using the \code{simsem} package
#'
#' @concept datasets
"df"
