#' @docType package
#' @name gentleman
#' @import dplyr
#' @import ggplot2
#' @import semTools
#' @importFrom tibble rownames_to_column column_to_rownames remove_rownames
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom broom tidy
#' @importFrom weights starmaker
#' @importFrom lavaan parameterEstimates lavNames lavPredictY
#' @importFrom fastDummies dummy_cols
#' @importFrom stats aov formula as.formula chisq.test contr.treatment lm na.pass sd setNames t.test
#' @importFrom utils combn write.csv2 write.table read.delim
#' @importFrom emmeans emmeans emtrends contrast emmip
#' @importFrom readxl read_excel
#' @importFrom cluster daisy pam
#' @importFrom NbClust NbClust
#' @importFrom h2o h2o.shutdown h2o.init h2o.varimp h2o.varimp_plot h2o.automl as.h2o h2o.predict
#' @importFrom parallel detectCores
#' @importFrom dataPreparation build_scales fast_scale
#' @importFrom psych outlier
#' @importFrom stringdist stringdistmatrix
#' @importFrom rcompanion cramerV
#' @importFrom QCA pofind truthTable minimize
#' @importFrom plyr rbind.fill
#' @importFrom stringr str_split
#'
#' @concept package
"_PACKAGE"
