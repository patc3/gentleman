#### model syntax ####
# generate lavaan model string for mediation model
#' Generate lavaan syntax for mediation model
#'
#' This function generates the \pkg{lavaan} syntax for a mediation model. The model can
#' take an arbitrary number of predictor (x), mediator (m), and outcome (y) variables.
#'
#' @param x Vector of predictor (x) variable names
#' @param med Vector of mediator (m) variable names
#' @param y Vector of outcome (y) variable names
#'
#' @return Character value to be used with \pkg{lavaan} as model syntax
#' @export
#'
#' @examples
#' library(lavaan)
#' get_mediation_model("x1", "x2", "x3") |>
#'    sem(df) |>
#'    summary()
get_mediation_model <- function(x, med, y)
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



#### clustering ####

#' Get cluster assignment using Gower distance matrix
#'
#' This function assigns each row of a data.frame to a cluster based on the Gower distance matrix,
#' and either a pre-specified or an optimal number of clusters.
#'
#' @details
#' First, a distance matrix is computed using [cluster::daisy()] with \code{metric="gower"}
#' and \code{stand=TRUE}. Next, clustering is performed around medoids (a more robust version of k-means
#' clustering) as implemented in [cluster::pam()].
#'
#' If no number of clusters \code{k} was specified, then the optimal
#' number of clusters is determined for the current distance matrix using [NbClust::NbClust()] with the
#' \code{method="median"} and \code{index="silhouette"}.
#'
#' @param df data.frame
#' @param v_cluster variables used to compute Gower distances between rows (if \code{NULL}, use all)
#' @param k number of clusters (if \code{NULL}, determined optimally; see Details)
#'
#' @return (factor) vector of cluster assignments (0 to k-1)
#' @export
#'
#' @examples
#' df |> get_cluster() |> table()
get_cluster <- function(df, v_cluster=NULL, k=NULL)
{
  # clustering vars
  if(is.null(v_cluster)) v_cluster <- names(df)

  # df and distance matrix
  df_cluster <- df |> select(all_of(v_cluster))
  distmat <- cluster::daisy(df_cluster, metric="gower", stand=TRUE)

  # choose number of clusters
  if(is.null(k))
  {
    (nbclust <- NbClust(diss=distmat, distance=NULL,
                        method=c("median", "ward.D2")[1],
                        index=c("all","silhouette","frey","mcclain","cindex","dunn")[2]))
    k <- nbclust$Best.nc["Number_clusters"] #6
    message("k chosen by NbClust():" |> paste(k))
  }

  # clustering using distance matrix
  # see documentation for pam() and advantages over kmeans
  pr <- pam(x = distmat, k = k, diss = TRUE)
  pr$clustering |> table() |> print()

  # cluster assignment
  (pr$clustering - 1) |> factor() # - 1 so that 0/1 when 2 clusters

}



#' Add cluster with iterative variable selection
#'
#' This function adds cluster assignment to each row of a data.frame with
#' iterative variable selection during clustering.
#'
#' @details
#' An initial set of clusters is determined using [get_cluster()] (Gower distance matrix with
#' clustering around medoids). Then, significant differences between clusters are determined using
#' [get_sig_differences_between_groups()] (ANOVA for numeric variables, Chi-square for categorical
#' variables), and clustering is performed again using the reduced set of significant variables.
#' This variable-reduction method is repeated until all remaining variables are significant,
#' or until \code{maxit} is reached. If \code{maxit} is reached before convergence, a warning is
#' thrown.
#'
#' When \code{maxit} is set to 0, a warning is thrown indicating that no variable selection
#' is performed, and this is equivalent to using [get_cluster()] directly.
#'
#' @param df data.frame
#' @param v_cluster (character) vector of variable names to use in clustering (if \code{NULL}, use all)
#' @param k (numeric) number of clusters (if \code{NULL}, determined optimally; see [get_cluster()])
#' @param maxit (numeric) maximum number of iterations to select significant outcomes of clusters
#' @param return_df_cluster_instead (logical) whether to return \code{df} with only final clustering
#' variables and cluster assignment (default \code{FALSE})
#' @param new_var_name (character) new variable name
#'
#' @return \code{df} with new cluster variable added
#' @export
#'
#' @examples
#' \dontrun{
#' df <- df |> add_cluster_assignment()
#'
#' df |>
#'    add_cluster_assignment(return_df_cluster_instead=TRUE) |>
#'    plot_density_by_groups(group="Cluster")
#' }
add_cluster_assignment <- function(df,
                                   v_cluster=NULL,
                                   k=NULL,
                                   maxit=10,
                                   return_df_cluster_instead=FALSE,
                                   new_var_name="Cluster")
{
  "
  this is a wrapper that starts off with all cluster vars,
  then picks only significant predictors of cluster,
  and re-estimates clusters with same k
  "

  # check
  if(maxit<0) stop("Max. iteration needs to be >= 0")
  if(maxit==0) warning("With maxit=0 there is no predictor selection; if return_df_cluster_instead=TRUE, equivalent to get_df_cluser()")

  # arg
  if(new_var_name %in% names(df)) stop("Variable" |> paste(new_var_name, "already in df"))
  message("Max. iteration:" |> paste(maxit))

  # get init clusters
  if(is.null(v_cluster)) v_cluster <- names(df)
  clusters <- v_cluster |>
    get_cluster(df=df, v_cluster=_, k=k)
  if(is.null(k)) k <- clusters |> unique() |> length()
  df[,new_var_name] <- clusters

  # use only sig vars to assign to clusters
  current_sig <- v_cluster
  updated_sig <- c()
  it <- 0
  while(!identical(updated_sig, current_sig))
  {
    if(it>maxit)
    {
      warning("Maximum iteration reached: Sig. predictors not converged")
      break
    }
    if(it>0)
    {
      current_sig <- updated_sig
      df[,new_var_name] <- current_sig |>
        get_cluster(df=df, v_cluster=_, k=k)
    }
    updated_sig <- df |> get_sig_differences_between_groups(test_vars=current_sig, group=new_var_name)
    print("Next iteration, removing from vars:");print(current_sig |> setdiff(updated_sig))
    #print("Next iteration, adding to vars:");print(updated_sig |> setdiff(current_sig))
    it <- it+1
  }
  if(identical(updated_sig, current_sig)) message("Sig. predictors converged")


  # out
  message("Added variable" |> paste(new_var_name))
  message("Predictor selection ran for" |> paste(it-1, "iteration(s)"))
  message("Final vars:")
  print(current_sig)
  if(return_df_cluster_instead) df |> select(all_of(c(current_sig, new_var_name))) else df

}
