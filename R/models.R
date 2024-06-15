#### model objects ####
#' Get model object(s) using formula interface
#'
#' This function generates a model formula using a vector of predictors
#' and a predictor link, and returns a list of models obtained from
#' calling a specified analysis function with the generated formula.
#' Additional arguments can also be passed to the analysis function.
#'
#' @details
#' The formula is constructed by surrounding each variable with backticks
#' to allow for special characters in variable names. The analysis function receives
#' the formula and the data.frame as first and second (positional) arguments,
#' respectively.
#'
#' @param df data.frame
#' @param dv (character) vector of outcome variable names
#' @param preds (character) vector of predictor variable names
#' @param preds_link link between predictors in model formula (default `+`)
#' @param ana_fn analysis function (for example: lm, lavaan::sem)
#' @param ... additional arguments passed to `ana_fn`
#'
#' @return Named list of objects returned by `ana_fn` with names `dv`
#' @export
#'
#' @examples
#' df |>
#'    get_fmodel(dv="y3", preds=c("x1","x2","x3"), ana_fn=lm) |>
#'    lapply(summary)
#'
#' @concept models
get_fmodel <- function(df, dv, preds, preds_link="+", ana_fn, ...)
{
  preds <- "`" %p% preds %p% "`"
  rhs <- preds %c% preds_link
  ana <- list()
  for(y in dv)
  {
    f <- "`" %p% y %p% "`" %P% "~" %P% rhs |> as.formula()
    ana[[y]] <- ana_fn(f, df, ...)
  }

  # out
  ana
}



#' Run qualitative comparative analysis (QCA)
#'
#' This function runs a QCA with necessary conditions, truth table, and
#' minimized solution(s). This is merely a wrapper using the \pkg{QCA} package.
#'
#' @param df data.frame
#' @param dv (character) outcome variable name
#' @param negate (logical) negate outcome? (default `FALSE`)
#' @param conditions (character) vector of variable names to use as conditions
#' @param n.cut (integer) minimum number of observations in each configuration
#' (if less than `n.cut`, observations are treated as remainders; default 1)
#' @param incl.cut (numeric) minimum coherence threshold (default .8, recommended
#' for fuzzy sets; another conventional value is .75 for crisp sets)
#' @param complex (logical) complex solution? (default `TRUE`) if `FALSE` and
#' `dir.exp` is `NULL` (default), the solution is parsimonious,
#' otherwise solution is intermediate
#' @param dir.exp (character) vector indicating directional expectations for each
#' condition (required for intermediate solution); negate conditions with `~`
#' (e.g. `c("cond1", "~cond2")`)
#' @param ... (optional) additional named arguments passed to [QCA::truthTable()]
#'
#' @return list with elements:
#' \describe{
#' \item{nec}{Necessary conditions (from calling [QCA::pofind()])}
#' \item{tt}{Truth table (from calling [QCA::truthTable()])}
#' \item{eq}{Minimized solution(s) (from calling [QCA::minimize()])}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' df |> get_qca(
#'    dv="numeric_transformation",
#'    negate=FALSE,
#'    conditions=c("RH", "RF", "RI", "RT"),
#'    n.cut=1,
#'    incl.cut=.8,
#'    complex=TRUE
#' )
#' }
#'
#' @concept models
get_qca <- function(df,
                    dv,
                    negate=FALSE,
                    conditions,
                    n.cut=1,
                    incl.cut=.8,
                    complex=TRUE,
                    dir.exp=NULL,
                    ...)
{
  # type of solution
  type <- if(complex) "complex" else
    if(is.null(dir.exp)) "parsimonious" else
      "intermediate"
  print("Type of solution:"%P%type)
  if(type=="intermediate") {
    print("Directional expectations:");
    print(dir.exp)
  } else if(complex) dir.exp<-NULL
  include <- if(type=="complex") "" else "?"

  # df: select only used vars & listwise
  df_qca<- df |>
    select(all_of(c(dv, conditions))) |>
    na.omit()

  # negate outcome?
  if(negate) dv <- "~"%p%dv

  # necessary conditions
  nec <- pofind(df_qca, dv, conditions)

  # truth table & minimization
  tt <- truthTable(data=df_qca,# |> replace_dot50(),
                   outcome = dv,
                   conditions = conditions,
                   incl.cut = incl.cut,
                   n.cut=n.cut, # to declare a remainder
                   sort.by = "incl",
                   ...)
  eq <- minimize(tt, details=T, include=include, dir.exp=dir.exp)
  list(nec=nec, tt=tt, eq=eq)
}




#### model syntax ####

#' Generate lavaan syntax for measurement model
#'
#' This function generates the \pkg{lavaan} syntax for a measurement model. The
#' factor names and items are taken from a named list. This can be used by
#' itself (e.g. for a CFA) or in conjunction with a structural model (e.g. with
#' a mediation model).
#'
#' @param factors_list Named list: each element is a vector of items
#'
#' @return Character value to be used with \pkg{lavaan} as model syntax
#' @export
#'
#' @examples
#' library(lavaan)
#' factors <- list(x="x"%p%1:3, y="y"%p%1:3)
#' factors |>
#'    get_measurement_model() |>
#'    sem(data=df) |>
#'    summary()
#'
#' @concept models
get_measurement_model <- function(factors_list)
{
  # measurement model
  model <- "# measurement model"
  for(f in names(factors_list)) model <- model%N%f%P%"=~"%P%(factors_list[[f]]%C%"+")
  model
}



#' Generate lavaan syntax for mediation model
#'
#' This function generates the \pkg{lavaan} syntax for a mediation model. The model can
#' take an arbitrary number of predictor (x), mediator (m), and outcome (y) variables.
#'
#' @details
#' This function implements the method used by JASP v0.16.4 to generate a mediation model,
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
#' JASP Team (2022). *JASP* (Version 0.16.4). Computer software:
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
    paste0(y[i_dv], " ~ ",
           paste0("b",i_dv, i_m, "*", m, collapse=" + "), " + ",
           paste0("c",i_dv, i_x, "*", x, collapse=" + "), "\n")

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
    # "# predictor covariance",
    # if(length(x)>1) paste0(apply(t(combn(x, 2)), 1, paste, collapse=" ~~ "), collapse="\n"), # necessary?
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
                                            v %P% "~" %P% (covariates %C% "+")) %c% "\n"


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


#' Generate lavaan syntax for 2-level mediation model
#'
#' This function generates the \pkg{lavaan} syntax for a two-level mediation model.
#' The model takes one predictor (x), one mediator (m), and one outcome (y).
#' The mediation model is either reproduced at both the within (level 1) and between
#' (level 2) levels, or the level-2 model is empty (corresponding to random-intercept
#' models in multilevel modeling). When the mediation is estimated at both levels, a test
#' of the difference between the level-1 and level-2 indirect effects is provided.
#'
#' @details
#' This model is for fully-nested designs and is described in Lachowicz, Sterba, & Preacher (2015).
#' The empty model at level 2 is saturated with covariances between endogeneous variables,
#' yielding a model similar to random-intercept models in multilevel modeling.
#'
#' @param x,m,y (character) variable names
#' @param empty_level2 (logical) should the level-2 model be left empty? (default `FALSE`;
#' see Details)
#'
#' @return Character value to be used with \pkg{lavaan} as model syntax
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' get_mediation_model_2level("x1", "x2", "x3") |>
#'    sem(df, group="id") |>
#'    summary()
#' }
#'
#' @references
#' Lachowicz, M. J., Sterba, S. K., & Preacher, K. J. (2015).
#' Investigating multilevel mediation with fully or partially nested data.
#' *Group Processes & Intergroup Relations*, *18*, 274–289.
#' \url{https://doi.org/10.1177/1368430214550343}.
#'
#' @concept models
get_mediation_model_2level <- function(x, m, y, empty_level2=FALSE)
{
  get_model_1level <- function(x,m,y,level)
  {
    "
    level:"%P%level%N%

      # mediation
      y%P%"~ b"%p%level%p%"*"%p%m%P%"+"%P%"c"%p%level%p%"*"%p%x%N%
      m%P%"~ a"%p%level%p%"*"%p%x%N%

      # effect decomposition
      "ind"%p%level%P%":= a"%p%level%p%"*b"%p%level%N%
      "tot"%p%level%P%":= ind"%p%level%P%"+ c"%p%level%N%

      # within-level variances
      x%P%"~~"%P%x%N%
      m%P%"~~"%P%m%N%
      y%P%"~~"%P%y
  }

  # mediation at both levels
  if(!empty_level2) model <- get_model_1level(x,m,y,1)%N%
      get_model_1level(x,m,y,2)%N%
      "diff := ind2-ind1"

  # mediation at level-1 only
  if(empty_level2) model <- get_model_1level(x,m,y,1)%N%"
      level: 2
    # saturated level (all vcov between endogenous vars)
    "%p%m%p%" ~~ "%p%m%p%" + "%p%y%p%"
    "%p%y%p%" ~~ "%p%y%p%"
  "

  # out
  model

}



# listwise deletion reproduces process() (with no resid correl or covariates)
# add_resid_correl=TRUE adds many parameters (exogeneous vars intercepts)
# this is because of FIML:
# lavaan WARNING: missing argument forces meanstructure = TRUE
#' Generate lavaan syntax for 3-wave longitudinal mediation model
#'
#' This function generates the \pkg{lavaan} syntax for a longitudinal mediation model
#' with three waves. The model uses a predictor x at time 1 (`x1`), one mediator at
#' times 2 and 3 (`m2`, `m3`), and one outcome at times 1, 2, and 3 (`y1`, `y2`, `y3`).
#'
#' @details
#' The indirect effect is computed as the effect from `x1` to `m2` to `y3`. The effect
#' of `x1` on `m2` is estimated after removing the effects of `m1` and `y1` on `m2`, and
#' the effect of `m2` on `y3` is estimated after removing the effects of `y1`, `y2`,
#' `x1`, and `m1` on `y3`. This corresponds to the model described in Hayes (2022),
#' Figure 14.4(B), p. 570.
#'
#' Optionally, residual correlations can be estimated between variables assessed
#' at the same time (as is conventional in cross-lagged models). It is also
#' possible to adjust for covariates, which is achieved by partialling
#' out the effect of the control variables given in `covariates` from `x1`,
#' `m2`, and `y3` (the variables involved in the indirect effect). This is
#' the method used by JASP v0.16.4 to adjust for covariates in mediation models.
#'
#' @references
#' Hayes, A. F. (2022).
#' *Introduction to mediation, moderation, and conditional process analysis* (3rd ed.).
#' New York: Guilford.
#'
#' JASP Team (2022). *JASP* (Version 0.16.4). Computer software:
#' \url{https://jasp-stats.org/}
#'
#'
#' @param x1 (character) variable name for predictor at time 1
#' @param m1,m2 (character) variable names for mediator at times 1-2
#' @param y1,y2,y3 (character) variable names for outcome at times 1-3
#' @param add_resid_correl (logical) whether to add residual correlations between variables
#' assessed at the same time (default `FALSE`)
#' @param covariates (character) covariate (control) variable names (default `NULL`)
#'
#' @return Character value to be used with \pkg{lavaan} as model syntax
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' get_mediation_model_3wave("x1", "m1", "m2", "y1", "y2", "y3") |>
#'    sem(df) |>
#'    summary()
#' }
#'
#' @concept models
get_mediation_model_3wave <- function(x1, m1, m2, y1, y2, y3, add_resid_correl=FALSE, covariates=NULL)
{
  model <- "
  "%p%m2%p%" ~ a1*"%p%x1%p%" + a2*"%p%m1%p%" + a3*"%p%y1%p%"
  "%p%y3%p%" ~ b1*"%p%m2%p%" + b2*"%p%m1%p%" + b3*"%p%y1%p%" + b4*"%p%y2%p%" + cp*"%p%x1%p%"

  # new params
  ind := a1*b1
  "
  if(add_resid_correl) model <- model %P% "
  # resid correl at each time
  "%p%x1%p%"~~"%p%m1%p%"
  "%p%x1%p%"~~"%p%y1%p%"
  "%p%m1%p%"~~"%p%y1%p%"
  "%p%m2%p%"~~"%p%y2

  # control variables (JASP method)
  model_control <- "# adjustment for control variables"
  if(!is.null(covariates)) for(v in c(x1, m2, y3))
    model_control <- c(model_control, v %P% "~" %P% (covariates %C% "+")) %c% "\n"

  # out
  model %N% model_control
}




#' Generate lavaan syntax for moderated mediation
#'
#' This function generates the \pkg{lavaan} syntax for a moderated mediation model
#' with optional moderators on the *a* (`x -> m`), *b* (`m -> y`), and *c*
#' (`x -> y`) paths. Optionally, covariates can be added, with effects partialled
#' out from endogeneous (`m`, `y`) and exogenous (`x`, moderators) variables.
#'
#' This function is a more flexible alternative to the *PROCESS* macro (Hayes, 2022),
#' which does not require manual specification of a model number. Also, because
#' the model is estimated in the structural equation modeling framework, both
#' normal-theory *p*-values and bootstrap intervals are available for indirect effects,
#' and additional parameters can be computed using the `:=` operator in \pkg{lavaan}.
#'
#' @details
#' If no moderators are specified, the model corresponds to the mediation model
#' provided in [get_mediation_model()].
#'
#' If only one of the *a* and *b* paths is moderated, then indexes of moderated
#' mediation are computed (labelled with the `i_` prefix in the output). If both
#' the *a* and *b* paths are moderated, indexes of moderated mediation are not
#' provided (similar to the *PROCESS* macro; see Hayes, 2022).
#'
#' Currently, the number of *x*, *m*, *y*, and *a*-*b*-*c* moderator variables is
#' limited to 9 each. This is not a statistical limitation, but instead due to
#' how the function is written.
#'
#' @references
#' Hayes, A. F. (2022).
#' *Introduction to mediation, moderation, and conditional process analysis* (3rd ed.).
#' New York: Guilford.
#'
#' @param x,m,y (character) variable name(s)
#' @param mod_a,mod_b,mod_c (character) variable name(s) for moderators of
#' *a* (`x -> m`), *b* (`m -> y`), and *c* (`x -> y`) paths (default `NULL`)
#' @param values_at list of numeric vectors with values at which to evaluate
#' the conditional effects; names correspond to `mod_a`, `mod_b`, and `mod_c`
#' (default `NULL`)
#' @param cov (character) variable name(s) of control variables (default `NULL`)
#' @param adjust_exogenous (logical) adjust exogenous variables (`x`, mods)
#' for the effect of covariates in `cov`? (default `TRUE`)
#'
#' @return Character value to be used with \pkg{lavaan} as model syntax
#' @export
#'
#' @examples
#' \dontrun{
#' library(lavaan)
#' get_moderated_mediation_model(
#'      x=c("x1", "x2"),
#'      m=c("m1", "m2"),
#'      y=c("y1", "y2", "y3"),
#'      mod_a=c("Sex","Ethnicity"),
#'      mod_b=NULL,
#'      mod_c="Sex",
#'      values_at=list(Sex=c(0,1), Ethnicity=c(0,1)),
#'      cov=c("IQ","Age")
#'    ) |>
#'    sem(df) |>
#'    summary()
#' }
#'
#' @concept models
get_moderated_mediation_model <- function(x,
                                          m,
                                          y,
                                          mod_a=NULL,
                                          mod_b=NULL,
                                          mod_c=NULL,
                                          values_at=NULL,
                                          cov=NULL,
                                          adjust_exogenous=TRUE)
{
  ##### checks #####
  # because use indexes, need only 1 digit for vars
  if(list(x,m,y,mod_a,mod_b,mod_c) |> sapply(length) |> (`>`)(9) |> any())
    stop("Currently max. 9 x, m, y, and a-b-c moderators allowed each")
  # mod values
  if((!c(mod_a,mod_b,mod_c)%in%names(values_at)) |> any())
    stop("`values_at` is a named list with names `mod_a`, `mod_b`, and `mod_c`")
  x <- x |> unique()
  m <- m |> unique()
  y <- y |> unique()
  mod_a <- mod_a |> unique()
  mod_b <- mod_b |> unique()
  mod_c <- mod_c |> unique()
  values_at <- values_at |> lapply(unique)
  cov <- cov |> unique()



  ##### mediator regression #####
  regressions <- ""
  for(i_m in 1:length(m))
  {
    regressions <- regressions %N%
      m[i_m]%P%"~"%P%
      ((sapply(1:length(x),\(i_x)"a"%p%i_m%p%i_x%p%"*"%p%x[i_x]))%C%"+")

    if(!is.null(mod_a))
      for(i_mod in 1:length(mod_a))
      {
        regressions <- regressions%P%"+ a"%p%i_m%p%"mod"%p%(i_mod)%p%"*"%p%mod_a[i_mod] # w
        for(i_x in 1:length(x))
          regressions <- regressions%P%
            "+ a"%p%i_m%p%i_x%p%"mod"%p%(i_mod)%p%"int*"%p%x[i_x]%p%":"%p%mod_a[i_mod] # xw
      }
  }


  ##### outcome regression #####
  for(i_y in 1:length(y))
  {
    regressions <- regressions %N%
      y[i_y]%P%"~"%P%
      ((sapply(1:length(m),\(i_m)"b"%p%i_y%p%i_m%p%"*"%p%m[i_m]))%C%"+")%P%"+"%P% # m
      ((sapply(1:length(x),\(i_x)"c"%p%i_y%p%i_x%p%"*"%p%x[i_x]))%C%"+") # x

    if(!is.null(mod_b))
      for(i_mod in 1:length(mod_b))
      {
        regressions <- regressions%P%"+ b"%p%i_y%p%"mod"%p%(i_mod)%p%"*"%p%mod_b[i_mod] # w
        for(i_m in 1:length(m))
          regressions <- regressions%P%
            "+ b"%p%i_y%p%i_m%p%"mod"%p%(i_mod)%p%"int*"%p%m[i_m]%p%":"%p%mod_b[i_mod]
      }

    if(!is.null(mod_c))
      for(i_mod in 1:length(mod_c))
      {
        regressions <- regressions%P%"+ c"%p%i_y%p%"mod"%p%(i_mod)%p%"*"%p%mod_c[i_mod] # w
        for(i_x in 1:length(x))
          regressions <- regressions%P%
            "+ c"%p%i_y%p%i_x%p%"mod"%p%(i_mod)%p%"int*"%p%x[i_x]%p%":"%p%mod_c[i_mod]
      }

  }


  ##### residual covariances ####
  # residual covariances
  residual_covs <- paste(
    # "# predictor covariance",
    # if(length(x)>1) paste0(apply(t(combn(x, 2)), 1, paste, collapse=" ~~ "), collapse="\n"), # necessary?
    "
# mediator residual covariance",
    if(length(m)>1) paste0(apply(t(combn(m, 2)), 1, paste, collapse=" ~~ "), collapse="\n"),
    "
# outcome residual covariance",
    if(length(y)>1) paste0(apply(t(combn(y, 2)), 1, paste, collapse=" ~~ "), collapse="\n"),
    "",
    sep="\n"
  )



  ##### conditional effects #####
  # helper fn to generate combinations of moderator values
  generate_combinations <- \(l)l |> expand.grid() |> arrange(across(everything()))

  path_conds <- list(a="",b="",c="")
  for(path in c("a","b","c"))
  {
    mods <- get("mod_"%p%path)
    path_vars <- if(path=="a")x else if(path=="b")m else if(path=="c")x # x ici?
    outcome_vars <- if(path=="a")m else if(path%in%c("b","c"))y
    if(!is.null(mods))
    {
      conditions <- values_at[mods] |> generate_combinations()
      conditions_map <- values_at[mods] |> lapply(\(v)1:length(v)) |> generate_combinations()
      for(i_out in 1:length(outcome_vars))
        for(i_v in 1:length(path_vars))
        {
          adj_to_path <- conditions |>
            apply(1,\(value)path%p%i_out%p%i_v%p%"mod"%p%(1:ncol(conditions))%p%"int*"%p%value%C%"+")
          for(i_cond in 1:nrow(conditions))
            path_conds[[path]] <- path_conds[[path]] %N%
              path%p%i_out%p%i_v%p%"_cond"%p%(conditions_map[i_cond,]%c%"")%P%
              ":="%P%path%p%i_out%p%i_v%P%"+"%P%adj_to_path[i_cond]
        }
    }
  }
  acond<-path_conds$a
  bcond<-path_conds$b
  ccond<-path_conds$c



  ##### indirect effects #####

  # all combinations of a and b conds
  extract_labels_and_conds <- \(cond)cond |>
    stringr::str_split("\n") |>
    unlist() |>
    (\(v)v[v!=""])() |>
    stringr::str_split(" := ",simplify=T) |>
    (\(m){m[,2]<-"("%p%m[,2]%p%")";m})()

  get_all_products <- \(a,b,op="*")b |> sapply(paste0,op,a) |> as.vector()

  # do
  make_cond_vector<-\(cond,letter,pred,outcome){
    vec<-letter%p%(expand.grid(1:length(outcome),1:length(pred)) |> apply(1,paste,collapse=""))
    vec%P%":="%P%vec%c%"\n"
  }

  if(acond=="") acond<-acond |> make_cond_vector("a",pred=x,outcome=m)
  if(bcond=="") bcond<-bcond |> make_cond_vector("b",pred=m,outcome=y)
  a<-acond |> extract_labels_and_conds()
  b<-bcond |> extract_labels_and_conds()
  a_values<-a[,2]
  b_values<-b[,2]
  a_labels<-a[,1]
  b_labels<-b[,1]
  ind_values<-a_values |> get_all_products(b_values)
  ind_labels<-a_labels |> get_all_products(b_labels,"_")

  # remove impossible combinations (keep only bym*amx, same m)
  # here and below is why vars are capped at 9 each (we use indexes)
  same_m <- ind_labels |>
    gsub(pattern="_cond.",replacement="") |> # remove conditional values
    sapply(\(v)substr(v,3,3)==substr(v,6,6))
  ind_labels <- ind_labels[same_m]
  ind_values <- ind_values[same_m]

  # remove impossible combinations if same mod in a-b paths
  if(!is.null(mod_a)&!is.null(mod_b)&any(mod_a%in%mod_b))
  {
    # # because use indexes, need only 1 digit for mod index and mod values
    if(values_at[c(mod_a,mod_b)] |> sapply(length) |> (`>`)(9) |> any())
      stop("Currently max. 9 values allowed for a-b moderators in `values_at` when they overlap")
    # if(length(mod_a)>9 | length(mod_b)>9)
    #   stop("Currently max. 9 a-b moderators allowed each when they overlap")

    # do
    ind_remove<-c()
    for(i_mod_a in 1:length(mod_a))
    {
      i_mod_b<-which(mod_a[i_mod_a]==mod_b)
      if(length(i_mod_b)>0)
      {
        ind_remove <- ind_labels |>
          strsplit("_") |>
          lapply(\(v)v[c(2,4)] |> gsub("cond","",x=_)) |>
          lapply(\(v)substr(v[2],i_mod_a,i_mod_a)!=
                   substr(v[1],i_mod_b,i_mod_b)) |>
          unlist() |>
          which() |>
          c(ind_remove) |>
          unique()
      }

    }
    ind_values<-ind_values[-ind_remove]
    ind_labels<-ind_labels[-ind_remove]

  }
  ind<-"ind_"%p%ind_labels%P%":="%P%ind_values
  ind<-ind%c%"\n"


  ###### indices of moderated mediation ####
  # for now only if a or b is not moderated
  # could extend to if length(mod_a |> setdiff(mod_b))==0
  ix_mod_med<-""
  mod_path <- c(!is.null(mod_a),!is.null(mod_b)) |> which()
  if(length(mod_path)==1)
  {
    mod_path <- ifelse(is.null(mod_a), "b", "a")
    nonmod_path <- ifelse(mod_path=="b", "a", "b")
    xory_mod <- ifelse(mod_path=="a","x","y")
    xory_nonmod <- ifelse(mod_path=="a","y","x")
    for(i_xory_nonmod in 1:length(get(xory_nonmod)))
      for(i_m in 1:length(m))
        for(i_xory_mod in 1:length(get(xory_mod)))
          for(i_mod in 1:length(get("mod_"%p%mod_path)))
          {
            ix_mod_med <- ix_mod_med %N%
              # label
              "i_"%p%xory_nonmod%p%i_xory_nonmod%p%
              "m"%p%i_m%p%
              xory_mod%p%i_xory_mod%p%
              "mod"%p%i_mod%P%

              # moderated path
              ":="%P%mod_path%p%
              ifelse(mod_path=="a",i_m,i_xory_mod)%p%
              ifelse(mod_path=="a",i_xory_mod,i_m)%p%
              "mod"%p%i_mod%p%"int*"%p%

              # non-moderated path
              nonmod_path%p%
              ifelse(mod_path=="a",i_xory_nonmod,i_m)%p%
              ifelse(mod_path=="a",i_m,i_xory_nonmod)
          }


  }



  ##### Covariates #####
  adj_by_cov <- ""
  if(!is.null(cov))
  {
    adjusted <- c(m,y)
    if(adjust_exogenous) adjusted <- adjusted |> c(x,mod_a,mod_c) |> unique()
    adj_by_cov <- (adjusted%C%"+")%P%"~"%P%(cov%C%"+")
  }


  ##### out #####
  regressions %N%
    path_conds$a %N%
    path_conds$b %N%
    path_conds$c %N%
    residual_covs %N%
    ix_mod_med %N%
    ind %N%
    adj_by_cov

}




#' Generate lavaan syntax for cross-lagged model
#'
#' This function generates the \pkg{lavaan} syntax for a cross-lagged panel model (CLPM).
#' With random intercepts, the syntax yields the random-intercepts cross-lagged panel
#' model (RI-CLPM). The function takes an arbitrary number of different variables,
#' and an arbitrary number of repeated assessments.
#'
#' @details
#' The RI-CLPM model requires a minimum of 3 assessments, and is described in
#' Hamaker et al (2015).
#' The corresponding model syntax is based on Mulder & Hamaker (2021).
#'
#' Direct paths are labelled using capital letters for variables and digits for time.
#' For example, the direct path between the first variable (`A`) at time 1 (`1`)
#' in `vars_list` and the third variable (`C`) at time 2 (`2`) is labelled `A1C2`.
#' With those labels, you can calculate custom parameters (like indirect effects)
#' using the `:=` syntax in \pkg{lavaan} (e.g.: `ind1 := A1B2*B2C3`).
#'
#' If `parallel_paths_equal` is set to `TRUE`, digits are removed from labels,
#' and parallel paths (between the same pairs of variables) are constrained to be
#' equal over time. For example, both `A1B2` and `A2B3` will become `AB`.
#'
#' @param vars_list List of vectors of variable names of length t (number of repeated
#' assessments)
#' @param random_intercepts (logical) Whether to add random intercepts (RI-CLPM model;
#' default `FALSE`)
#' @param parallel_paths_equal (logical) Whether to fix parallel paths (between
#' the same pairs of variables at different times) to equality (default `FALSE`)
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
get_crosslagged_model <- function(vars_list,
                                  random_intercepts=FALSE,
                                  parallel_paths_equal=FALSE)
{
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
      paths <- vars_list[[i_v]][t] %P% "~" %P% # lhs
        (LETTERS[1:length(vars_list)] %p%
           ifelse(parallel_paths_equal, "", (t-1)) %p%
           LETTERS[i_v] %p%
           ifelse(parallel_paths_equal, "", t) %p% "*" %p% # labels
           (vars_list |> sapply(\(v)v[t-1])) %C% "+") # rhs
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
#' If `model` is of class `lavaan`, an additional argument `lavaan.DV` must be
#' passed to specify the name of the outcome variable. Transfer of model
#' information to \pkg{emmeans} is handled by [semTools::lavaan2emmeans()].
#'
#' @param model A model object compatible with \code{emmeans} (e.g. \code{lm})
#' @param df data.frame or NULL if \code{model} has \code{model$model}
#' @param x1,x2 (character) variable name
#' @param at_x1,at_x2 (numeric or character vector) \code{x1} and \code{x2} values at which to calculate
#' and compare means (if \code{NULL}, all unique values)
#' @param ci (logical) Whether to plot confidence intervals (default `FALSE`)
#' @param ... additional arguments passed to the \pkg{emmeans} functions (see Details)
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
                                  ci=FALSE,
                                  ...)
{
  if(inherits(model,"lavaan")&!"lavaan.DV"%in%names(list(...)))
    stop("Need to provide outcome variable name in `lavaan.DV`")
  if(is.null(df)) df <- model$model
  if(is.null(at_x1)) at_x1 <- unique(df[,x1])
  if(is.null(at_x2)) at_x2 <- unique(df[,x2])
  values <- list(at_x1, at_x2) |> setNames(c(x1, x2))
  diff <- emtrends(model,
                   "pairwise ~" |> paste(x1) |> formula(),
                   var=x2,
                   at=values,
                   adjust="none",
                   ...)
  means <- emmeans(model,
                   "~" |> paste(paste(x2,x1,sep="*")) |> formula(),
                   at=values,
                   ...)
  contrasts <- contrast(means, "pairwise", by=x2, adjust="none")

  # plot
  p_data <- emmip(model, x1 |> paste(x2, sep="~") |> formula(), CIs=TRUE, at=values, plotit=FALSE, ...)
  p_data[,x1] <- p_data[,x1] |> factor()
  p <- ggplot(p_data, aes_string(x=x2, y="yvar")) + geom_line(aes_string(linetype=x1), linewidth=1)
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



#' Get interaction plot for lavaan model
#'
#' This function generates an interaction plot from a \pkg{lavaan} model
#' with a 2-way (x*w) interaction.
#'
#' @details
#' The predicted values are calculated using [lavaan::lavPredictY()] for the
#' variable given in `y`, and observed variables are extracted from the
#' `model` object using [lavaan::lavNames()]. All other variables are assumed
#' to be 0, so the *y*-axis on the plot will only be meaningful if all
#' observed variables were centered prior to entering the model.
#'
#' @param model \pkg{lavaan} object
#' @param x (character) variable name to plot on the *x* axis
#' @param w (character) variable name to plot on separate lines (moderator)
#' @param y (character) variable name indicating the outcome
#' @param values_at (named list) values at which to plot `x` and `w`
#' (default -1, 0, and 1 for both)
#'
#' @return `ggplot2` plot
#' @export
#'
#' @examples
#' library(lavaan)
#' "y2 ~ x1 + x2 + x1:x2" |>
#'    sem(df) |>
#'    get_lavaan_interaction_plot(x="x1", w="x2", y="y2")
#'
#' @concept models
get_lavaan_interaction_plot<-function(model,
                                      x,
                                      w,
                                      y,
                                      values_at=NULL)
{
  if(values_at |> is.null())
    values_at<-list(c(-1,0,1),
                    c(-1,0,1)) |>
      setNames(c(x,w))
  if((!c(x,w)%in%names(values_at)) |> any())
    stop("`values_at` is a named list with names `x`, `w`")
  values_at<-values_at[c(x,w)]


  ynames<-lavNames(model, "ov.y")
  endo<-lavNames(model, "ov.nox")
  xnames<-lavNames(model,"ov") |> setdiff(ynames)
  ixn<-lavNames(model,"ov.interaction")
  ov<-model |> lavNames("ov") |> setdiff(c(ixn))
  df_pred<-empty_df(c(xnames,endo) |> unique())
  conds<- values_at |> expand.grid()
  df_pred[1:nrow(conds),]<-0
  df_pred[names(conds)]<-conds

  # calc ixn
  for(pair in strsplit(ixn,":"))
    df_pred[pair%c%":"]<-df_pred[,pair[1]]*df_pred[,pair[2]]

  # pred
  preds<-model |> lavPredictY(newdata=df_pred,
                              ynames=y,
                              xnames=xnames |> setdiff(y))
  df_pred<-df_pred[!colnames(df_pred)%in%colnames(preds)] |> cbind(preds)

  # plot
  p<-ggplot(df_pred,aes(x=get(x),y=get(y)))+
    geom_line(aes(linetype=factor(get(w))),linewidth=1)
  p <- p + labs(x=x, y=y, linetype=w, fill=w)
  p <- p+theme(
    text=element_text(size=20),
    panel.border = element_rect(colour = "black", fill=NA),
    panel.grid=element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(),
    legend.position=NULL#c(.85,.5)#legend.position
  )
  p

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
    rename(Outcome=col)

  if(nrow(sig)==0)
    sig <- data.frame(matrix(nrow=0,ncol=3)) |> setNames(c("Outcome", "Predictor", "Sign"))
  else
  {
    sig <- sig |> select(Outcome, Predictor)

    # add sign
    sig$Sign <- NA
    for(i in 1:nrow(sig)) sig$Sign[i] <- table |>
      filter(Predictor == sig$Predictor[i]) |>
      select(all_of(sig$Outcome[i])) |>
      substr(1,1)
    sig$Sign <- (sig$Sign == "-") |> ifelse("-", "+")
  }

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
    lapply(\(t) if(nrow(t)>0)
      t |>
        get_sig_effects_from_pub_table(pattern=pattern, fixed=fixed) |>
        (\(t_sig)
         if(nrow(t_sig)>0) with(t_sig, paste0(Outcome, ": ", Predictor, " (", Sign, ")"))
         else character())()
    )

  # output
  cat("_______________________________\n")
  cat(pattern |> paste("in table1 but not in table2:\n"))
  (!sig[[1]] %in% sig[[2]]) |> (\(v)sig[[1]][v])() |> print()

  cat("_______________________________\n")
  cat(pattern |> paste("in table2 but not in table1:\n"))
  (!sig[[2]] %in% sig[[1]]) |> (\(v)sig[[2]][v])() |> print()
}
