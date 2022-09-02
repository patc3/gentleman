#### model syntax ####
# generate lavaan model string for mediation model
#' Generate lavaan syntax for mediation model
#'
#' This function generates the \pkg{lavaan} syntax for a mediation model. The model can
#' take an arbitrary number of predictor (x), mediator (m), and outcome (y) variables.
#'
#' @details
#' This function implements the method used by JASP v0.16.3 to generate a mediation model,
#' possibly adjusting for covariates. Adjustment for covariates is done by partialling
#' out the effect of the control variables given in `covariates` from all predictor `x`,
#' mediator `m`, and outcome `y` variables.
#'
#' @param x Vector of predictor (x) variable names
#' @param m Vector of mediator (m) variable names
#' @param y Vector of outcome (y) variable names
#' @param covariates Vector of covariate (control) variable names (default `NULL`)
#'
#' @return Character value to be used with \pkg{lavaan} as model syntax
#' @export
#'
#' @examples
#' library(lavaan)
#' get_mediation_model("x1", "x2", "x3") |>
#'    sem(df) |>
#'    summary()
#'
#' @references
#' JASP Team (2022). *JASP* (Version 0.16.3). Computer software:
#' \url{https://jasp-stats.org/}
#'
#' @concept models
get_mediation_model <- function(x, m, y, covariates=NULL)
{
  # structural model
  i_m <- 1:length(m)
  i_x <- 1:length(x)
  i_y <- 1:length(y)

  # outcome regression
  model_outcome_regression <- "# outcome regression\n"
  for(i_dv in i_y) model_outcome_regression <- model_outcome_regression |>
    paste0(y[i_dv], " ~ ", paste0("b",i_dv, i_m, "*", m, collapse=" + "), " + ", paste0("c",i_dv, i_x, "*", x, collapse=" + "), "\n")

  # effect decomposition
  model_effect_decomposition <- "# effect decomposition\n"
  for(i_dv in 1:length(y))
  {
    for(i_pred in i_x)
    {
      model_effect_decomposition <- model_effect_decomposition |>
        paste0("# ", y[i_dv], " ~ ", x[i_pred], "
", paste0("ind_x",i_pred,"_m", i_m, "_y",i_dv," := a", i_m, i_pred, "*b",i_dv, i_m, collapse="\n"), "
ind_x",i_pred,"_y",i_dv," := ", paste0("ind_x",i_pred,"_m", i_m, "_y",i_dv, collapse=" + "), "
tot_x",i_pred,"_y",i_dv," := ind_x",i_pred,"_y",i_dv," + c",i_dv,i_pred,"
")
    }

  }

  # mediator regression
  model_mediator_regression <- "# mediator regression\n"
  for(i_med in i_m) model_mediator_regression <- model_mediator_regression |>
    paste0(m[i_med], " ~ ", paste0("a",i_med, i_x, "*", x, collapse=" + "), "\n")

  # residual covariances
  model_residual_covariances <- paste(
    "# predictor residual covariance",
    if(length(x)>1) paste0(apply(t(combn(x, 2)), 1, paste, collapse=" ~~ "), collapse="\n"), # necessary?
    "
# mediator residual covariance",
    if(length(m)>1) paste0(apply(t(combn(m, 2)), 1, paste, collapse=" ~~ "), collapse="\n"),
    "
# outcome residual covariance",
    if(length(y)>1) paste0(apply(t(combn(y, 2)), 1, paste, collapse=" ~~ "), collapse="\n"),
    "",
    sep="\n"
  )


  # control variables
  model_control <- "# adjustment for control variables"
  if(!is.null(covariates)) for(v in c(x, m, y)) model_control <- c(model_control,
                                            v %P% "~" %P% (covariates %c% " + ")) %c% "\n"


  # model
  model <- paste(
    model_outcome_regression,
    model_mediator_regression,
    model_residual_covariances,
    model_effect_decomposition,
    model_control,
    sep="\n"
  )


  return(model)
}



# generate lavaan model string from list of variables
#' Generate lavaan syntax for cross-lagged model
#'
#' This function generates the \pkg{lavaan} syntax for a cross-lagged panel model (CLPM).
#' With random-intercepts, the syntax yields the random-intercepts cross-lagged panel
#' model (RI-CLPM). The function takes an arbitrary number of different variables,
#' and an arbitrary number of repeated assessments.
#'
#' @details
#' The RI-CLPM model requires a minimum of 3 assessments, and is described in
#' Hamaker et al (2015).
#' The corresponding model syntax is based on Mulder & Hamaker (2021).
#'
#' @param vars_list List of vectors of variable names of length t (number of repeated assessments)
#' @param random_intercepts (logical) Whether to add random intercepts (RI-CLPM model)
#'
#' @return Character value to be used with \pkg{lavaan} as model syntax
#' @export
#'
#' @examples
#' library(lavaan)
#' vars <- list(
#'    c("x1", "x2", "x3"),
#'    c("y1", "y2", "y3")
#' )
#' get_crosslagged_model(vars) |>
#'    sem(df) |>
#'    summary()
#'
#' @references
#' Hamaker, E. L., Kuiper, R. M., & Grasman, R. P. P. P. (2015).
#' A critique of the cross-lagged panel model. \emph{Psychological Methods},
#' \emph{20}, 102-116.
#'
#' Mulder, J. D., & Hamaker, E. L. (2021).
#' Three extensions of the random intercept cross-lagged panel model.
#' \emph{Structural Equation Modeling: A Multidisciplinary Journal}.
#' \url{https://doi.org/10.1080/10705511.2020.1784738}.
#'
#' @concept models
get_crosslagged_model <- function(vars_list, random_intercepts=FALSE)
{
  "
  vars_list is list of vectors of vars of length t
      ex.: list(c('x1','x2','x3'), c('y1', 'y2', 'y3')
  random_intercepts is RI-CLPM
  "

  # init model
  model <- ""

  # set number of times
  n_times <- length(vars_list[[1]])

  # with random intercepts?
  if(random_intercepts)
  {
    # between components: random intercepts
    between <- "# Between: Random intercepts\n"
    vars_ri <- c() # list of random intercepts created
    for(vars in vars_list)
    {
      name <- vars[1]# |> substr(start=1, stop=3)
      ri <- "RI_" |> paste0(name) # set name of latent var
      vars_ri <- vars_ri |> c(ri) # add to list of random intercepts
      ri_loadings <- ri |>
        paste("=~ ") |>
        paste0("1*" |> paste0(vars, collapse=" + "))
      between <- between |> paste(ri_loadings, sep="\n")
    }

    # covariances (L2)
    l2_cov <- get_all_pairs(vars_ri) |>
      apply(1, paste, collapse=" ~~ ") |>
      paste(collapse="\n")
    l2_var <- vars_ri |> paste(vars_ri, sep=" ~~ ", collapse="\n")
    between <- between |> paste(l2_var, l2_cov, sep="\n")


    # within components
    within <- "# Within"
    vars_w <- list()
    for(i_vars in 1:length(vars_list))
    {
      vars <- vars_list[[i_vars]]
      name <- vars[1]# |> substr(start=1, stop=3)
      w <- "w_" |> paste0(name)
      w_loadings <- ""
      vars_w[[i_vars]] <- vector()
      for(t in 1:length(vars))
      {
        w_name <- w |> paste0(t)
        vars_w[[i_vars]] <- vars_w[[i_vars]] |> c(w_name)
        w_loadings_current <- w_name |>
          paste("=~ ") |>
          paste0("1*", vars[t])
        w_loadings <- w_loadings |> paste(w_loadings_current, sep="\n")
      }
      within <- within |> paste(w_loadings, sep="\n")
    }



    # add to model
    model <- model |> paste(between, within, sep="\n\n")


    # change vars_list to use new within vars
    vars_list <- vars_w

  }

  # correlations
  correlations <- get_all_pairs_with_op(vars_list, "~~")

  # direct paths
  direct_paths <- NULL
  for(t in 2:n_times)
  {
    for(i_v in 1:length(vars_list))
    {
      paths <- vars_list[[i_v]][t] |> paste(paste0(vars_list |> sapply(\(v)v[t-1]), collapse=" + "), sep=" ~ ")
      direct_paths <- direct_paths |> paste(paths, sep="\n")
    }
  }

  # (residual) variances (explicitly)
  vars <- vars_list |> unlist()
  variances <- vars |> paste(vars, sep=" ~~ ", collapse="\n")


  model <- paste(model,
                 "
  #correlations
  "
                 ,paste0(correlations)
                 ,"
  #stabilities & cross lags (i.e. direct paths)
  "
                 ,paste0(direct_paths)
                 ,"
  # (residual) variances (explicitly declared)
  "
                 ,paste0(variances)
                 ,sep="\n")

  return(model)

}


#### process models ####
#' Decompose 2-way (x1*x2) interaction
#'
#' This function decomposes a 2-way x1*x2 interaction into simple effects,
#' and provides means, pairwise comparisons, and a plot.
#'
#' @details
#' This function uses the facilities provided by the library \pkg{emmeans}, and
#' follows the procedure outlined by
#' [UCLA OARC Statistical and Data Analytics](https://stats.oarc.ucla.edu/r/seminars/interactions-r/).
#' It might be helpful to execute the function with x1 and x2 inverted, and see which
#' set of results and plot make most sense for your scenario.
#'
#' Note that if \code{x1} or \code{x2} has many different distinct values and \code{at_x1}
#' or \code{at_x2} is not specified, then not all elements of the returned list will be useful
#' (\code{$means} and \code{$contrasts} in particular may be difficult to use).
#'
#' @param model A model object compatible with \code{emmeans} (e.g. \code{lm})
#' @param df data.frame or NULL if \code{model} has \code{model$model}
#' @param x1,x2 (character) variable name
#' @param at_x1,at_x2 (numeric or character vector) \code{x1} and \code{x2} values at which to calculate
#' and compare means (if \code{NULL}, all unique values)
#' @param ci (logical) Whether to plot confidence intervals
#'
#' @return List with elements:
#' \describe{
#' \item{slopes}{Simple slopes and pairwise comparisons (from calling \code{emmeans::emtrends()})}
#' \item{means}{(adjusted) means for every combination of \code{x1*x2}
#' (may need to specify \code{at_x1} and/or \code{at_x2} values)}
#' \item{contrasts}{Comparisons of (adjusted) \code{x1} means at each value of \code{x2}
#' (may need to specify \code{at_x1} and/or \code{at_x2} values)}
#' \item{plot}{Plot (\code{ggplot2} line graph using data from \code{emmeans::emmip()})}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- lm(SelfEsteem ~ Gender*AttitudeTowardsSchool, df)
#' fit |> decompose_interaction(x1="Gender", x2="AttitudeTowardsSchool")
#' }
#'
#' @seealso [emmeans::emmeans()], [emmeans::emtrends()], [emmeans::emmip()]
#'
#' @concept models
decompose_interaction <- function(model,
                                  df=NULL,
                                  x1,
                                  x2,
                                  at_x1=NULL,
                                  at_x2=NULL,
                                  ci=FALSE)
{
  if(is.null(df)) df <- model$model
  if(is.null(at_x1)) at_x1 <- unique(df[,x1])
  if(is.null(at_x2)) at_x2 <- unique(df[,x2])
  values <- list(at_x1, at_x2) |> setNames(c(x1, x2))
  diff <- emtrends(model, "pairwise ~" |> paste(x1) |> formula(), var=x2, at=values)
  means <- emmeans(model, "~" |> paste(paste(x2,x1,sep="*")) |> formula(), at=values)
  contrasts <- contrast(means, "pairwise", by=x2)

  # plot
  p_data <- emmip(model, x1 |> paste(x2, sep="~") |> formula(), CIs=TRUE, at=values, plotit=FALSE)
  p_data[,x1] <- p_data[,x1] |> factor()
  p <- ggplot(p_data, aes_string(x=x2, y="yvar")) + geom_line(aes_string(linetype=x1), size=1)
  if(ci) p <- p + geom_ribbon(aes_string(ymax="UCL", ymin="LCL", fill=x1), alpha=0.4)
  p <- p + labs(x=x2, y="Estimated Outcome", linetype=x1, fill=x1)
  p <- p+theme(
    text=element_text(size=20),
    panel.border = element_rect(colour = "black", fill=NA),
    panel.grid=element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(),
    legend.position=NULL#c(.85,.5)#legend.position
  )

  # out
  list(slopes=diff, means=means, contrasts=contrasts, plot=p)
}



#### publication tables ####
# make table to copy-paste in excel
#' Generate publication table from lavaan models
#'
#' @description
#' This function generates a publication table from a list of lavaan models
#' to be exported or copy-pasted (e.g. into Excel). The table shows all
#' parameters with operator "~" (i.e. regression paths) with significance level:
#' \itemize{
#' \item{\code{***}}{p < .001}
#' \item{\code{**}}{p < .01}
#' \item{\code{*}}{p < .05}
#' \item{\code{+}}{p < .10}
#' }
#'
#' Each column is a different outcome variable/lavaan model, each row is
#' a different predictor.
#'
#' @param ana list of lavaan models
#' @param check_same_format (logical) whether to force all models to have the same predictors
#' @param transpose (logical) whether to transpose the table (default `FALSE`)
#'
#' @return data.frame with predictors and coefficients (and significance level) in each model
#' @export
#'
#' @examples
#' library(lavaan)
#' ana <- list()
#' ana$y1 <- sem("y1 ~ x1+x2", df)
#' ana$y2 <- sem("y2 ~ x1+x2", df)
#' ana |> make_pub_table_from_lavaan_models()
#'
#' @concept models
make_pub_table_from_lavaan_models <- function(ana,
                                              check_same_format=TRUE,
                                              transpose=FALSE)
{
  "
  input: ana list (list of lavaan models)
  out: df with IVs and estimate+pvalue for each target
  "

  pe <- lapply(ana, parameterEstimates)
  pe <- lapply(pe, function(x) x[x$op=="~",])

  # check  that all summaries have the same format
  if(check_same_format)
  {
    same_row_orders <- lapply(pe, function(x) all(x[,"rhs"] == pe[[1]][,"rhs"]))
    if (!all(unlist(same_row_orders))) stop("Not all summaries have the same format")
  }

  # get estimates, p-value from each model
  out <- subset(pe[[1]], select=rhs) |> rename(Predictor=rhs)
  for(s in pe)
  {
    # postproc p-value
    s$sig <- weights::starmaker(s$pvalue, symbols=c("***", "**", "*", "+"))

    # concat est (2 decimals) and sig
    s$est_sig <- paste0(sprintf('%.2f',s$est), s$sig)

    # store in table
    v_out <- c("est", "pvalue", "sig", "est_sig")[4]
    out[,ncol(out)+1:length(v_out)] <- s[,v_out]


  }

  # colnames
  if(length(v_out)==1 & !is.null(names(ana))) names(out)[2:ncol(out)] <- names(ana)

  # transpose?
  if(transpose) out <- out |> transpose_df()

  #out
  return(out)
}


# make table to copy-paste in excel from broom::tidy() output
#' Generate publication table from broom::tidy summaries
#'
#' @description
#' This function generates a publication table from a list of tidy summaries
#' to be exported or copy-pasted (e.g. into Excel). The table shows all
#' coefficients in the 'term' column with significance level:
#' \itemize{
#' \item{\code{***}}{p < .001}
#' \item{\code{**}}{p < .01}
#' \item{\code{*}}{p < .05}
#' \item{\code{+}}{p < .10}
#' }
#'
#' Each column is a different outcome variable/tidy summary, each row is
#' a different predictor.
#'
#' @param ana list of tidy summaries
#' @param transpose (logical) whether to transpose the table (default `FALSE`)
#'
#' @return data.frame with predictors and coefficients (and significance level) in each model
#' @export
#'
#' @examples
#' library(broom)
#' ana <- list()
#' ana$y1 <- lm(y1 ~ x1+x2, df) |> tidy()
#' ana$y2 <- lm(y2 ~ x1+x2, df) |> tidy()
#' ana |> make_pub_table_from_broom_tidy()
#'
#' @concept models
make_pub_table_from_broom_tidy <- function(ana, transpose=FALSE)
{
  "
  input: ana list (list of tidy outputs)
  out: df with IVs and estimate+pvalue for each target
  "
  # make dfs
  ana <- ana |> lapply(as.data.frame)

  # get estimates, p-value from each model
  out <- ana[[1]][c("term")]
  for(i in 1:length(ana))
  {
    # ith analysis
    a <- ana[[i]]
    y <- names(ana)[i]

    # postproc p-value
    a$sig <- weights::starmaker(a$p.value, symbols=c("***", "**", "*", "+"))

    # concat est (2 decimals) and sig
    a[,y] <- paste0(sprintf('%.2f',a$estimate), a$sig)

    # store in table
    a <- a[,c("term", y)]
    out <- out |> full_join(a, by="term")

  }

  # rename as Predictor
  out <- out |> rename(Predictor=term)

  # transpose?
  if(transpose) out <- out |> transpose_df()

  #out
  return(out)
}



# get list of significant/trend effects from a pub table
#' Extract significant effects from a publication table
#'
#' This function extracts the significant effects from a publication table generated by any
#' of the \code{make_pub_table_from_...()} functions.
#'
#' @param table Publication generated for lavaan models or tidy summaries
#' @param pattern Regular expression used to find significant effects (default is any * or +)
#' @param fixed (logical) Whether pattern is to be searched as is (TRUE)
#' or as a regular expression (FALSE)
#'
#' @return data.frame with columns Outcome, Predictor, and Sign
#' (indicating whether the coefficient is positive or negative)
#' @export
#'
#' @examples
#' library(lavaan)
#' ana <- list()
#' ana$y1 <- sem("y1 ~ x1+x2", df)
#' ana$y2 <- sem("y2 ~ x1+x2", df)
#' pub <- ana |> make_pub_table_from_lavaan_models()
#' pub |> get_sig_effects_from_pub_table()
#'
#' @concept models
get_sig_effects_from_pub_table <- function(table, pattern="\\*|\\+", fixed=F)
{
  # get significant predictors with outcome
  sig <- table |>
    remove_rownames() |>
    column_to_rownames("Predictor") |>
    apply(1:2, grepl, pattern=pattern, fixed=fixed) |>
    which(arr.ind=T) |>
    (\(m)(data.frame(Predictor=rownames(m), m, row.names=NULL)))() |>
    select(-row) |>
    mutate(col=names(table |> select(-1))[col]) |>
    rename(Outcome=col) |>
    select(Outcome, Predictor)

  # add sign
  sig$Sign <- NA
  for(i in 1:nrow(sig)) sig$Sign[i] <- table |>
    filter(Predictor == sig$Predictor[i]) |>
    select(all_of(sig$Outcome[i])) |>
    substr(1,1)
  sig$Sign <- (sig$Sign == "-") |> ifelse("-", "+")

  # out
  return(sig)
}


# compare sig effects from two pub tables
#' Compare significant effects from two publication tables
#'
#' This function prints to console which effects are significant in table1 but not in table2,
#' and which are significant in table2 but not in table1. The two tables are
#' publication tables generated by any of the \code{make_pub_table_from_...()} functions
#'
#' @param table1 A publication table
#' @param table2 Another publication table
#' @param pattern Pattern used to determine significant effects (default is *)
#' @param fixed (logical) Whether pattern is to be searched as is (TRUE)
#' or as a regular expression (FALSE)
#'
#' @export
#'
#' @examples
#' library(lavaan)
#' ana <- list()
#' ana$y1 <- sem("y1 ~ x1+x2", df, missing="ml.x")
#' ana$y2 <- sem("y2 ~ x1+x2", df, missing="ml.x")
#' pub_sem <- ana |> make_pub_table_from_lavaan_models()
#'
#'library(broom)
#' ana <- list()
#' ana$y1 <- lm(y1 ~ x1+x2, df) |> tidy()
#' ana$y2 <- lm(y2 ~ x1+x2, df) |> tidy()
#' pub_lm <- ana |> make_pub_table_from_broom_tidy()
#'
#' # sensitivity analysis: effect of missing data
#' pub_lm |> compare_sig_effects_in_two_pub_tables(pub_sem)
#'
#' @concept models
compare_sig_effects_in_two_pub_tables <- function(table1, table2, pattern="*", fixed=T)
{
  # get significant effects in both tables
  sig <- list(table1, table2) |>
    lapply(\(t) t |>
             get_sig_effects_from_pub_table(pattern=pattern, fixed=fixed) |>
             with(paste0(Outcome, ": ", Predictor, " (", Sign, ")")))

  # output
  cat("_______________________________\n")
  cat(pattern |> paste("in table1 but not in table2:\n"))
  (!sig[[1]] %in% sig[[2]]) |> (\(v)sig[[1]][v])() |> print()

  cat("_______________________________\n")
  cat(pattern |> paste("in table2 but not in table1:\n"))
  (!sig[[2]] %in% sig[[1]]) |> (\(v)sig[[2]][v])() |> print()
}
