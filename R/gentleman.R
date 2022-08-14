#' gentleman: Helpers for Data Preparation, Descriptives, Models, & Publication
#'
#' This package provides a suite of modules to help with data preparation,
#' descriptive statistics, modeling, and publication formatting.
#' The Descriptives module is extensible to accommodate custom functions
#' to generate tables and p-values (or other useful statistic to compare groups).
#' The Models module can be used to generate lavaan model strings for mediation
#' and cross-lagged models.
#'
#' @author Patrick Coulombe \email{patrick.coulombe@d22consulting.com}
#' @docType package
#' @name gentleman
#' @import dplyr
#' @importFrom tibble rownames_to_column column_to_rownames remove_rownames
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom broom tidy
#' @importFrom weights starmaker
#' @importFrom lavaan parameterEstimates
#' @importFrom fastDummies dummy_cols
#' @importFrom stats aov as.formula chisq.test contr.treatment lm na.pass sd setNames t.test
#' @importFrom utils combn write.csv2 write.table
"_PACKAGE"
