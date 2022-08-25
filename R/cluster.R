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
#'
#' @concept cluster
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
#' When \code{maxit} is set to 0, a warning is thrown indicating that no variable selection
#' is performed, and this is equivalent to using [get_cluster()] directly.
#'
#' @param df data.frame
#' @param v_cluster (character) vector of variable names to use in clustering (if \code{NULL}, use all)
#' @param k (numeric) number of clusters (if \code{NULL}, determined optimally; see [get_cluster()])
#' @param maxit (numeric) maximum number of iterations to select significant outcomes of clusters
#' @param elimination (character) whether to use backward or bidirectional elimination
#' (see Details; default `backward`)
#' @param return_df_cluster_instead (logical) whether to return \code{df} with only final clustering
#' variables and cluster assignment (default \code{FALSE})
#' @param new_var_name (character) new variable name
#'
#' @return \code{df} with new cluster variable added
#' @export
#'
#' @seealso [get_cluster()], [get_sig_differences_between_groups()]
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
                                   maxit=10,
                                   elimination=c("backward", "bidirectional"),
                                   return_df_cluster_instead=FALSE,
                                   new_var_name="Cluster")
{
  # check
  if(maxit<0) stop("Max. iteration needs to be >= 0")
  if(maxit==0) warning("With maxit=0 there is no predictor selection; if return_df_cluster_instead=TRUE, equivalent to get_df_cluser()")

  # arg
  if(new_var_name %in% names(df)) stop("Variable" |> paste(new_var_name, "already in df"))
  elimination <- match.arg(elimination)
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
    print("Iteration #" |> paste(it))
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
    test_vars <- switch(elimination, backward=current_sig, bidirectional=v_cluster)
    updated_sig <- df |> get_sig_differences_between_groups(test_vars=test_vars, group=new_var_name)
    print("Next iteration, removing from vars:");print(current_sig |> setdiff(updated_sig))
    print("Next iteration, adding to vars:");print(updated_sig |> setdiff(current_sig))
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
