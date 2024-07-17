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
#' @param weights (numeric vector) variable weights for calculating Gower distances (default all 1)
#'
#' @return (factor) vector of cluster assignments (0 to k-1)
#' @export
#'
#' @examples
#' df |> get_cluster() |> table()
#'
#' @concept cluster
get_cluster <- function(df,
                        v_cluster=NULL,
                        k=NULL,
                        weights=NULL)
{
  # clustering vars
  if(is.null(v_cluster)) v_cluster <- names(df)
  if(is.null(weights)) weights <- rep.int(1, length(v_cluster))
  if(is.null(names(weights))) names(weights) <- v_cluster
  print(weights)

  # df and distance matrix
  df_cluster <- df |> select(all_of(v_cluster))
  distmat <- cluster::daisy(df_cluster, metric="gower", stand=TRUE, weights=weights)

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
  pr <- cluster::pam(x = distmat, k = k, diss = TRUE)
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
#' variables), and clustering is performed again using the set of significant variables (and the same `k`).
#' This variable-selection method is repeated until all remaining variables are significant,
#' or until \code{maxit} is reached. If \code{maxit} is reached before convergence, a warning is
#' thrown.
#'
#' Variable selection can be achieved either through **backward** elimination, where the full set
#' of candidate variables is first considered, then only significant predictors are kept in the
#' next iteration, and so on until all variables are significant. This guarantees that each iteration
#' will have a reduced set of variables.
#'
#' Alternatively, variable selection can be achieved through **bidirectional** elimination, where
#' at each step the full set of initial variables is tested for differences between clusters,
#' whether or not they were included in the previous iteration. All significant predictors are
#' included in the next iteration. This guarantees that the final set of variables will match
#' the output of [get_sig_differences_between_groups()] with `test_vars=v_cluster` and `group=new_var_name`
#' (provided convergence has been reached).
#'
#' If `max_vars_rm_or_add_each_it` is less than the number of variables to add or remove at a given
#' iteration, then this sets the number of changes made to the variable set for that iteration,
#' separately for variables removed and variables added (if elimination is bidirectional). For example,
#' if `max_vars_rm_or_add_each_it` is set to 1, then at each iteration, only one variable can be removed,
#' and only one variable can be added. If several candidates are available, then which variable(s)
#' get added or removed are selected randomly. This can be useful in determining cluster variable
#' importances using `calc_cluster_importances()`.
#'
#' When \code{maxit} is set to 0, a warning is thrown indicating that no variable selection
#' is performed, and this is equivalent to using [get_cluster()] directly.
#'
#' @param df data.frame
#' @param v_cluster (character) vector of variable names to use in clustering (if \code{NULL}, use all)
#' @param k (numeric) number of clusters (if \code{NULL}, determined optimally; see [get_cluster()])
#' @param maxit (numeric) maximum number of iterations to select significant outcomes of clusters
#' @param elimination (character) whether to use backward or bidirectional elimination
#' (see Details; default `backward`)
#' @param max_vars_rm_or_add_each_it (positive int) max number of variables to add and remove (each)
#' at each iteration (see Details; default `Inf`)
#' @param return_df_cluster_instead (logical) whether to return \code{df} with only final clustering
#' variables and cluster assignment (default \code{FALSE})
#' @param new_var_name (character) new variable name
#' @param weights (numeric vector) variable weights for calculating Gower distances (default all 1)
#'
#' @return \code{df} with new cluster variable added
#' @export
#'
#' @seealso [get_cluster()], [get_sig_differences_between_groups()], [calc_cluster_importances()]
#'
#' @examples
#' \dontrun{
#' df <- df |> add_cluster_assignment()
#'
#' df |>
#'    add_cluster_assignment(return_df_cluster_instead=TRUE) |>
#'    plot_density_by_groups(group="Cluster")
#' }
#'
#' @concept cluster
add_cluster_assignment <- function(df,
                                   v_cluster=NULL,
                                   k=NULL,
                                   maxit=100,
                                   elimination=c("backward", "bidirectional"),
                                   max_vars_rm_or_add_each_it=Inf,
                                   return_df_cluster_instead=FALSE,
                                   new_var_name="Cluster",
                                   weights=NULL)
{
  # check
  if(maxit<0) stop("Max. iteration needs to be >= 0")
  if(maxit==0) warning("With maxit=0 there is no predictor selection; if return_df_cluster_instead=TRUE, equivalent to get_df_cluser()")

  # arg
  if(new_var_name %in% names(df)) stop("Variable" |> paste(new_var_name, "already in df"))
  elimination <- match.arg(elimination)
  message("Elimination:" |> paste(elimination))
  message("Max. iteration:" |> paste(maxit))

  # track clusters
  clusters <- list()


  # get init clusters
  if(is.null(v_cluster)) v_cluster <- names(df)
  if(is.null(weights)) weights <- rep.int(1, length(v_cluster))
  if(is.null(names(weights))) names(weights) <- v_cluster
  clusters[[1]] <- v_cluster |> get_cluster(df=df, k=k, weights=weights[v_cluster])
  if(is.null(k)) k <- clusters[[1]] |> unique() |> length()
  df[,new_var_name] <- clusters[[1]]


  # use only sig vars to assign to clusters
  current_sig <- v_cluster
  updated_sig <- c()
  it <- 0


  # variable selection
  while(!setequal(updated_sig, current_sig))
  {
    message("Iteration #" |> paste(it))
    if(it>maxit)
    {
      warning("Maximum iteration reached: Sig. predictors not converged")
      break
    }


    # update clusters
    if(it>0)
    {
      current_sig <- updated_sig
      current_sig <- sample(current_sig) # randomize order
      clusters[[it+1]] <- current_sig |> get_cluster(df=df, k=k, weights=weights[current_sig])
      df[,new_var_name] <- clusters[[it+1]]
    }


    # get significant outcomes
    test_vars <- switch(elimination, backward=current_sig, bidirectional=v_cluster)
    updated_sig <- df |> get_sig_differences_between_groups(test_vars=test_vars, group=new_var_name)


    # define changes in significant outcomes vs. previous iteration
    v_change <- list()
    v_change$no_longer_pred <- current_sig |> setdiff(updated_sig)
    v_change$new_pred <- updated_sig |> setdiff(current_sig)


    # check for stuck in loop & no n.s. predictors to remove
    if(it>=3 &&
       clusters[[it+1]] |> identical(clusters[[it-1]]) &&
       length(v_change$no_longer_pred) == 0) # no n.s. vars to remove
    {
      message("Cluster assignments repeated from 2 iterations ago and
              clusters significantly different on all variables;
              Stopping now")
      break
    }


    # restrict changes next iteration as requested
    if(max_vars_rm_or_add_each_it>0 &&
       any(max_vars_rm_or_add_each_it > (v_change |> sapply(length))))
    {
      v_change_adjusted <- v_change |>
        lapply(\(v) if(max_vars_rm_or_add_each_it < length(v)) v |>
                 sample(size=max_vars_rm_or_add_each_it)
               else v)


      # adjust updated sig
      for(i in 1:length(v_change)) v_change[[i]] <- v_change[[i]] |>
          replace_in_vector(find=setdiff(v_change[[i]], v_change_adjusted[[i]]),
                            replace=NULL)
      updated_sig <- current_sig |> # start from current sig
        replace_in_vector(find=v_change$no_longer_pred, NULL) |> # remove selected no longer preds
        c(v_change$new_pred) # add new selected preds
      #updated_sig <- v_cluster[updated_sig |> match(v_cluster) |> sort()] # to preserve original order
    }


    # print and increase
    print("Next iteration, removing from vars:");print(v_change$no_longer_pred)
    print("Next iteration, adding to vars:");print(v_change$new_pred)
    it <- it+1
  }


  # check convergence
  if(setequal(updated_sig, current_sig)) message("Sig. predictors converged")


  # out
  current_sig <- v_cluster[current_sig |> match(v_cluster) |> sort()] # preserve original order
  message("Added variable" |> paste(new_var_name))
  message("Predictor selection ran for" |> paste(it-1, "iteration(s)"))
  message("Final vars:")
  print(current_sig)
  if(return_df_cluster_instead) df |> select(all_of(c(current_sig, new_var_name))) else df

}



#' Calculate variable importances in clustering
#'
#' This function calculates variable importances when clustering with variable selection.
#'
#' @details
#' Variable importances are estimated through simulation with `nrep` calls to `add_cluster_assignment()`
#' with the specified configuration via `...`. The function retrieves the number of times each
#' variable was kept at the final step of clustering.
#'
#' @param df data.frame
#' @param nrep (positive int) number of repetitions in the simulation (default `100`)
#' @param ... arguments passed to `add_cluster_assignment()`
#'
#' @return (table) percentage of times each variable was used in the final clustering step
#' @export
#'
#' @examples
#' \dontrun{
#' df |>
#'   calc_cluster_importances(k=2,
#'                            maxit=100,
#'                            elimination="bidirectional",
#'                            max_vars_rm_or_add_each_it=1)
#' }
#' @concept cluster
#'
#' @seealso [add_cluster_assignment()]
calc_cluster_importances <- function(df, nrep=100, ...)
{
  vars <- list()
  for (i in 1:nrep)
  {
    message("\n****************** Cluster Importances: Iteration #"%p%i%p%"/"%p%nrep%p%"\n")
    try(sink("NUL"), silent = TRUE)
    vars[[i]] <- df |>
      add_cluster_assignment(return_df_cluster_instead = TRUE,
                             new_var_name = "_gntlmn_cluster",
                             ...) |>
      select(-`_gntlmn_cluster`) |>
      names()
    sink()
  }
  vars |> unlist() |> table() |> sort(decreasing = TRUE) |> (\(t)t/nrep*100)()

}
