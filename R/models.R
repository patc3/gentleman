#### model syntax ####
# generate lavaan model string for mediation model
#' Generate lavaan syntax for mediation model
#'
#' This function generates the lavaan syntax for a mediation model. The model can
#' take an arbitrary number of predictor (x), mediator (m), and outcome (y) variables.
#'
#' @param x Vector of predictor (x) variable names
#' @param med Vector of mediator (m) variable names
#' @param y Vector of outcome (y) variable names
#'
#' @return Character value to be used with lavaan as model syntax
#' @export
#'
#' @examples
#' library(lavaan)
#' get_mediation_model("x1", "x2", "x3") |>
#'    sem(df) |>
#'    summary()
get_mediation_model <- function(x=c("x1", "x2"), med=c("m1", "m2"), y=c("y1", "y2"))
{
  # structural model
  i_m <- 1:length(med)
  i_x <- 1:length(x)
  i_y <- 1:length(y)

  # outcome regression
  model_outcome_regression <- "# outcome regression\n"
  for(i_dv in i_y) model_outcome_regression <- model_outcome_regression |>
    paste0(y[i_dv], " ~ ", paste0("b",i_dv, i_m, "*", med, collapse=" + "), " + ", paste0("c",i_dv, i_x, "*", x, collapse=" + "), "\n")

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
    paste0(med[i_med], " ~ ", paste0("a",i_med, i_x, "*", x, collapse=" + "), "\n")

  # residual covariances
  model_residual_covariances <- paste(
    "# predictor residual covariance",
    if(length(x)>1) paste0(apply(t(combn(x, 2)), 1, paste, collapse=" ~~ "), collapse="\n"), # necessary?
    "
# mediator residual covariance",
    if(length(med)>1) paste0(apply(t(combn(med, 2)), 1, paste, collapse=" ~~ "), collapse="\n"),
    "
# outcome residual covariance",
    if(length(y)>1) paste0(apply(t(combn(y, 2)), 1, paste, collapse=" ~~ "), collapse="\n"),
    "",
    sep="\n"
  )

  # model
  model <- paste(
    model_outcome_regression,
    model_mediator_regression,
    model_residual_covariances,
    model_effect_decomposition,
    sep="\n"
  )


  return(model)
}



# generate lavaan model string from list of variables
#' Generate lavaan syntax for cross-lagged model
#'
#' This function generates the lavaan syntax for a cross-lagged panel model (CLPM).
#' With random-intercepts, the syntax yields the random-intercepts cross-lagged panel
#' model (RI-CLPM). The function takes an arbitrary number of different variables,
#' and an arbitrary number of repeated assessments.
#'
#' @param vars_list List of vectors of variable names of length t (number of repeated assessments)
#' @param random_intercepts (logical) Whether to add random-intercepts (RI-CLPM model)
#'
#' @return Character value to be used with lavaan as model syntax
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
# I think bin could simply be categorical
# Also cont cannot be truly continuous!
#' Decompose binary*continuous interaction
#'
#' This function decomposes a binary*continuous interaction into simple effects,
#' and provides means, pairwise comparisons, and a plot.
#'
#' @details
#' This function uses the facilities provided by the library \code{emmeans}, and
#' follows the procedure outlined by
#' [UCLA OARC Statistical and Data Analytics](https://stats.oarc.ucla.edu/r/seminars/interactions-r/).
#'
#' Note that if \code{cont} is truly continuous (many different distinct values),
#' then not all elements of the returned list will be useful (\code{$means} and
#' \code{$contrasts} in particular may be difficult to use).
#'
#' @param model A model object compatible with \code{emmeans} (e.g. \code{lm})
#' @param df data.frame or NULL if \code{model} has \code{model$model}
#' @param bin Binary variable name
#' @param cont Continuous variable name
#' @param ci (logical) Whether to plot confidence intervals
#'
#' @return List with elements:
#' \describe{
#' \item{\code{slopes}}{Simple slopes and pairwise comparisons (from calling \code{emmeans::emtrends()})}
#' \item{\code{means}}{(adjusted) means for every combination of \code{bin*cont}
#' (may not be useful if \code{cont} is truly continuous)}
#' \item{\code{contrasts}}{Comparisons of (adjusted) \code{bin}
#' means at each value of \code{cont} (may not be useful if \code{cont} is truly continuous)}
#' \item{\code{plot}}{Plot (from calling \code{emmeans::emmip()})}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- lm(SelfEsteem ~ Gender*AttitudeTowardsSchool, df)
#' fit |> decompose_ixn_bin_cont(bin="Gender", cont="AttitudeTowardsSchool")
#' }
decompose_ixn_bin_cont <- function(model, df=NULL, bin, cont, ci=FALSE)
{
  if(is.null(df)) df <- model$model
  diff <- emtrends(model, "pairwise ~" |> paste(bin) |> formula(), var=cont)
  values <- list(unique(df[,cont]), unique(df[,bin])) |> setNames(c(cont, bin))
  means <- emmeans(model, "~" |> paste(paste(cont,bin,sep="*")) |> formula(), at=values)
  contrasts <- contrast(means, "pairwise", by=cont)
  p <- emmip(model, bin |> paste(cont, sep="~") |> formula(), CIs=ci, at=values)

  # out
  list(slopes=diff, means=means, contrasts=contrasts, plot=p)
}
