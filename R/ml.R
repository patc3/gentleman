#### config ####
#' Create configuration object for running autoML
#'
#' This function creates the configuration object
#' required to execute [get_automl_model()] and
#' [run_automl_pipeline()].
#'
#' @param include_algos (character) vector of included algorithms (see [h2o::h2o.automl()])
#' @param max_runtime_secs maximum time in seconds for H2O's AutoML to run (see [h2o::h2o.automl()])
#' @param max_models maximum number of models
#' @param max_runtime_secs_per_model maximum time in seconds for each model
#' @param balance_classes (logical) whether to balance classes for a categorical target (see [h2o::h2o.automl()])
#'
#' @return Configuration object (list) to pass to [get_automl_model()] or [run_automl_pipeline()]
#' @export
#'
#' @examples
#' \dontrun{
#' config <- get_automl_config()
#' pip <- df |> run_automl_pipeline(target="y1", config)
#' }
#'
#' @seealso
#' [get_automl_model()], [run_automl_pipeline()], [h2o::h2o.automl()]
#'
#' @concept machine_learning
get_automl_config <- function(include_algos="DRF",
                              max_runtime_secs=NULL,
                              max_models=1,
                              max_runtime_secs_per_model=60*5,
                              balance_classes=FALSE)
{
  # create config
  config <- list(
    include_algos=include_algos
    ,max_runtime_secs = max_runtime_secs
    ,max_models=max_models
    ,max_runtime_secs_per_model=max_runtime_secs_per_model
    ,balance_classes=balance_classes
  )

  # class
  class(config) <- "gentleman.config" |> c(class(config))

  # out
  config
}






#### train test ####
#' Split data.frame into training and test sets
#'
#' This function splits a data.frame into training and test sets.
#'
#' @param df data.frame
#' @param prop_train proportion of rows to be allocated to training
#'
#' @return list with elements:
#' \describe{
#' \item{train}{\code{df} with \code{prop_train*100}% rows sampled at random}
#' \item{test}{\code{df} with remaining rows}
#' }
#'
#' @export
#'
#' @examples
#' tt <- df |> ttsplit()
#'
#' @seealso
#' [scale_numeric_features_in_train_and_test()],
#' [keep_only_vars_in_both_train_and_test()],
#' [remove_target_from_test_and_add_ref_to_env()],
#' [add_target_back_to_test_set_from_ref_table()]
#'
#' @concept machine_learning
ttsplit <- function(df, prop_train=.7)
{
  df_split <- list()
  ix <- sample(nrow(df), round(nrow(df)*prop_train), replace=F)
  df_split$train <- df[ix,]
  df_split$test <- df[-ix,]
  print(paste0("Split df into list with: train, test (proportion train = ", prop_train, ")"))
  return(df_split)
}


#' Scale numeric features in train and test sets
#'
#' This function scales numeric features in the training set,
#' and uses the scaling information from the training set
#' (mean and variance) to scale the corresponding variables
#' in the test set.
#'
#' @details
#' Scaling the test set using variable information in the training set
#' is performed to avoid target leakage, and to avoid evaluating model
#' performance on a test set that uses information that might not be available
#' at test time (i.e. mean and variance of variables for other test observations).
#'
#' @param tt train-test list (see [ttsplit()])
#'
#' @return \code{tt} with numeric features scaled
#' @export
#'
#' @examples
#' tt <- df |>
#'    ttsplit() |>
#'    scale_numeric_features_in_train_and_test()
#'
#' @seealso
#' [dataPreparation::build_scales()], [ttsplit()]
#'
#' @concept machine_learning
scale_numeric_features_in_train_and_test <- function(tt)
{
  # scale numeric features train & test
  v_num <- colnames(tt$train)[which(sapply(tt$train, class) %in% c("numeric", "integer"))]
  scales <- dataPreparation::build_scales(data_set = tt$train, cols=v_num, verbose=TRUE)
  tt$train <- dataPreparation::fast_scale(data_set = tt$train, scales = scales, verbose = TRUE) |> as.data.frame()

  # test
  tt$test <- dataPreparation::fast_scale(data_set = tt$test, scales=scales, verbose=TRUE) |> as.data.frame()

  # out
  print("Created numerical scales using train set and rescaled numerical features in train and test")
  return(tt)
}


#' Remove variables from train and test sets when they are unique
#'
#' This function removes variables from train and test sets when they
#' are unique to either the training or test set. Removing variables from the
#' training set if they cannot be found in the test set is particularly
#' useful to avoid training a model on information that will not be
#' available at testing time.
#'
#' @param tt train-test list (see [ttsplit()])
#' @param remove_from_train_only (logical) whether to leave the test set untouched
#'
#' @return \code{tt} with unique variables removed from the train (and potentially test) set(s)
#' @export
#'
#' @examples
#' tt <- df |>
#'    ttsplit() |>
#'    keep_only_vars_in_both_train_and_test()
#'
#' @seealso
#' [ttsplit()]
#'
#' @concept machine_learning
keep_only_vars_in_both_train_and_test <- function(tt, remove_from_train_only=FALSE)
{
  v_in_common <- Reduce(intersect, lapply(tt, names))#set(names(tt$train, tt$test))
  if(!remove_from_train_only) tt <- lapply(tt, function(df) df[,v_in_common]) else tt$train <- tt$train[,v_in_common]
  print(paste0("Selected only variables in common ", ifelse(remove_from_train_only, "in train set (test set unchanged)", "in both train and test sets")))
  return(tt)

}





# prevent target leakage
#' Remove target from test set and back up values in reference table
#'
#' This function removes the target values from the test set (replacing with \code{NA})
#' and backs up the values in a new variable in the global environment. This is particularly
#' useful to avoid target leakage (i.e. accidentally using the target value during testing).
#'
#' @param tt train-test list (see [ttsplit()])
#' @param target (character) name of target variable
#' @param unique_id (character) name of unique ID variable
#' @param ref_name (character) name of new variable that stores mapping between ID and target
#'
#' @return \code{tt} with target replaced with \code{NA} in \code{$test},
#' and \code{ref_name} added to global environment
#' @export
#'
#' @examples
#' tt <- df |>
#'    dplyr::mutate(id=1:nrow(df)) |>
#'    ttsplit() |>
#'    remove_target_from_test_and_add_ref_to_env("y1", "id", "target_values")
#'
#' @seealso
#' [add_target_back_to_test_set_from_ref_table()]
#'
#' @note This function implements the method described in
#' [Preventing Target Leakage](https://en.d22consulting.com/quantcafe/preventing-target-leakage) (D22 QuantCafé, 2021).
#'
#' @concept machine_learning
remove_target_from_test_and_add_ref_to_env <- function(tt, target, unique_id, ref_name)
{
  ref_table <- data.frame(id=tt$test[,unique_id], target=tt$test[,target])
  colnames(ref_table)[1] <- unique_id # same as test set
  colnames(ref_table)[2] <- target # same as test set
  tt$test[,target] <- NA
  print(paste0("Target column ", target, " replaced with NA in test set"))
  assign(ref_name, value=ref_table, envir = .GlobalEnv)
  print(paste0("Added reference table '", ref_name, "' to global environment"))
  return(tt)
}



# add actual target back to test set
#' Add target values back in the test set
#'
#' This function adds back the target values in the test set using
#' a reference table.
#'
#' @details
#' The reference table is a data.frame initially created by
#' [remove_target_from_test_and_add_ref_to_env()].
#'
#' @param tt train-test list (see [ttsplit()])
#' @param ref_table reference table (see Details)
#'
#' @return \code{tt} with target replaced with original values in \code{$test}
#' @export
#'
#' @examples
#' \dontrun{
#' tt <- tt |> add_target_back_to_test_set_from_ref_table(target_values)
#' }
#'
#' @seealso
#' [remove_target_from_test_and_add_ref_to_env()]
#'
#' @note This function implements the method described in
#' [Preventing Target Leakage](https://en.d22consulting.com/quantcafe/preventing-target-leakage) (D22 QuantCafé, 2021).
#'
#' @concept machine_learning
add_target_back_to_test_set_from_ref_table <- function(tt, ref_table)
{
  id_var <- names(ref_table)[1]
  target_var <- names(ref_table)[2]
  tt$test[,target_var] <- ref_table[,target_var][match(x=tt$test[,id_var], table=ref_table[,id_var])]
  print(paste0("Replaced values in target '", target_var, "' with values from reference table"))
  return(tt)
}





#### h2o ####
#' Initialize a local H2O cluster
#'
#' This function initializes a local H2O cluster for use with the \pkg{h2o}
#' machine learning library.
#'
#' @details
#' The number of available threads is determined using \code{parallel::detectCores()}. The
#' \code{nthreads} option needs to be one of:
#' * \code{half}: Half of all available cores (the default)
#' * \code{minus1}: All available threads minus 1 (to avoid freezing)
#' * \code{all}: All available cores (\emph{not recommended}; this can freeze your machine)
#'
#' The H2O cluster requires Java to be installed on your machine.
#' Normally the graphical interface to interact with the cluster is available by
#' browser at \code{localhost:54321}. Refer to the
#' [\pkg{h2o} documentation](https://docs.h2o.ai/h2o/latest-stable/h2o-docs/faq/r.html)
#' for more information.
#'
#' @param nthreads (character) how many threads (cores) to dedicate to the H2O cluster (see Details)
#'
#' @return New local cluster initialized (return value is given by [h2o::h2o.init()])
#' @export
#'
#' @examples
#' \dontrun{
#' init_h2o() # half cores
#' init_h2o("minus1") # all but one cores
#' }
#'
#' @seealso
#' [h2o::h2o.init()], [parallel::detectCores()]
#'
#' @concept machine_learning
init_h2o <- function(nthreads=c("half", "minus1", "all"))
{
  # get input
  nthreads <- match.arg(nthreads)

  # thread options
  nthreads_values <- list(half=parallel::detectCores() / 2, all=-1)
  nthreads_values <- c(nthreads_values, list(minus1=nthreads_values$half*2-1))

  # shutdown
  try({
    print("Shutting down any previous h2o clusters")
    h2o.shutdown(F)
    Sys.sleep(2)
  }, silent=TRUE)

  # init
  nthreads <- nthreads_values[[nthreads]]
  h2o.init(nthreads=nthreads, max_mem_size = paste0(floor(.7*16), "g"))

}



#' Send train-test data.frames to H2O cluster
#'
#' This function sends the train and test data.frames to the current
#' H2O cluster.
#'
#' @details
#' Upon running this function, the two data.frames should be
#' available to the local H2O cluster, and should be accessible
#' via the graphical interface for the cluster (normally
#' available through a browser at \code{localhost:54321}) or using
#' the R API as documented in the \pkg{h2o} package.
#'
#' @param tt train-test list (see [ttsplit()])
#'
#' @return H2O pointers to the H2O frames (needed to interact with the cluster
#' using the R API through the \pkg{h2o} package)
#' @export
#'
#' @examples
#' \dontrun{
#' init_h2o()
#' tt <- df |> ttsplit()
#' tt_h2o <- tt |> ship_train_and_test_to_h2o()
#' }
#'
#' @seealso
#' [run_automl_pipeline()], [h2o::as.h2o()]
#'
#' @concept machine_learning
ship_train_and_test_to_h2o <- function(tt)
{
  # train-test
  train <- as.h2o(tt$train, "train")
  test <- as.h2o(tt$test, "test")
  print("Destination frames added: 'train' and 'test'")

  # check
  nrow_r <- tt |> lapply(nrow)
  nrow_h2o <-list(train=as.data.frame(train), test=as.data.frame(test)) |> lapply(nrow)
  if(!identical(nrow_r, nrow_h2o))
  {
    print("H2O frames:"); print(nrow_h2o)
    print("R dfs:"); print(nrow_r)
    stop("H2O frames don't have the same number of rows!")
  }

  # out
  return(list(train=train, test=test))

}



#' Run an H2O AutoML model
#'
#' This function is a wrapper for [h2o::h2o.automl()],
#' with relevant options specified through the configuration
#' object generated by [get_automl_config()].
#'
#' @details
#' Only \code{$train} is currently used in \code{tt_h2o}.
#'
#' If a configuration object is not provided, the default configuration
#' is the configuration generated by [get_automl_config()].
#'
#' @param tt_h2o list of H2O pointers to H2O frames (see [ship_train_and_test_to_h2o()])
#' @param target (character) name of the target variable
#' @param config configuration object (see [get_automl_config()])
#' @param ... additional arguments passed to [h2o::h2o.automl()]
#'
#' @return AutoML object from H2O
#' @export
#'
#' @examples
#' \dontrun{
#' init_h2o()
#' tt <- df |> ttsplit()
#' tt_h2o <- tt |> ship_train_and_test_to_h2o()
#' automl <- tt_h2o |> get_automl_model(target="y1")
#' }
#'
#' @seealso
#' [get_automl_config()], [h2o::h2o.automl()]
#'
#' @concept machine_learning
get_automl_model <- function(tt_h2o,
                             target,
                             config=get_automl_config(),
                             ...)
{
  # model
  automl <- h2o.automl(
    y=target,
    training_frame = tt_h2o$train,
    include_algos = config$include_algos,
    max_runtime_secs = config$max_runtime_secs
    ,max_models = config$max_models
    ,max_runtime_secs_per_model = config$max_runtime_secs_per_model
    ,balance_classes=config$balance_classes
    ,...
  )

  # out
  return(automl)
}



# add predictions
#' Add predictions from an AutoML object to a test set
#'
#' This function generates predictions from an AutoML object for the test
#' set in a train-test split, and adds the predictions to the test set.
#' Confidence levels can also optionally be added alongside predictions.
#'
#' @param automl H2O AutoML object (from [h2o::h2o.automl()] or [get_automl_model()])
#' @param tt train-test list (see [ttsplit()])
#' @param tt_h2o H2O pointers to train and test frames (see [ship_train_and_test_to_h2o()])
#' @param add_confidence_level (logical) whether to add confidence levels
#' alongside predictions in test set (default \code{TRUE})
#'
#' @return train-test list with predictions (and optionally confidence levels) added to \code{$test}
#' @export
#'
#' @examples
#' \dontrun{
#' init_h2o()
#' tt <- df |> ttsplit()
#' tt_h2o <- tt |> ship_train_and_test_to_h2o()
#' automl <- tt_h2o |> get_automl_model(target="y1")
#' tt <- add_predictions_from_automl(automl)
#' }
#'
#' @seealso
#' [h2o::h2o.predict()], [h2o::h2o.automl()], [get_automl_model()]
#'
#' @concept machine_learning
add_predictions_from_automl <- function(automl, tt, tt_h2o, add_confidence_level=TRUE)
{
  leader_pred <- h2o.predict(automl, newdata = tt_h2o$test)
  leader_pred_df <- as.data.frame(leader_pred)
  colnames(leader_pred_df) <- colnames(leader_pred)
  df_confidences <- leader_pred_df[,-1]

  tt$test$predict <- leader_pred_df$predict

  # out
  print("Added predict column to test")

  # confidence levels if requested
  if (add_confidence_level)
  {
    tt$test$confidence <- apply(df_confidences, 1, max)
    print("Added confidence column to test")
  }


  # return
  return(tt)

}



#var imp plot
#' Plot and print variable importances from an AutoML model
#'
#' This function plots and prints variable importances from an AutoML
#' model (if available), and returns variable importances (\code{data.frame}).
#'
#' @param automl H2O AutoML model (from calling [h2o::h2o.automl()] or [get_automl_model()])
#'
#' @return \code{data.frame} of variable importances
#' @export
#'
#' @examples
#' \dontrun{
#' init_h2o()
#' tt <- df |> ttsplit()
#' tt_h2o <- tt |> ship_train_and_test_to_h2o()
#' automl <- tt_h2o |> get_automl_model(target="y1")
#' var_imp <- automl |> plot_and_print_variable_importances()
#' }
#'
#' @seealso
#'[h2o::h2o.varimp()], [h2o::h2o.varimp_plot()], [h2o::h2o.automl()], [get_automl_model()]
#'
#' @concept machine_learning
plot_and_print_variable_importances <- function(automl)
{
  try({
    h2o.varimp_plot(automl@leader)
    print(as.data.frame(h2o.varimp(automl@leader)))
  }, silent=F)
}



#### pipeline ####
#' Run a full AutoML pipeline
#'
#' This function runs a full AutoML pipeline, starting from a \code{data.frame}
#' split into training and test sets to adding predictions from an AutoML model
#' to the test set and possibly reporting variable importances.
#'
#' @details
#' The \code{df} data.frame is first prepared by removing non-ASCII characters,
#' casting all character variables to factors (if any), and removing blank factor
#' levels from factors. Then it is split into training and test sets, and if requested,
#' numeric features are scaled based on sample statistics in the training set. Next,
#' a local H2O cluster is launched, the train and test sets are uploaded to the cluster,
#' and an AutoML model is requested using parameters specified in the configuration
#' object. Predictions are added to the test set (with or without confidence levels),
#' and variable importances may be plotted and reported. Finally, if requested,
#' the H2O cluster is shut down.
#'
#' If a configuration object is not provided, the default configuration
#' is the configuration generated by [get_automl_config()].
#'
#' @param df data.frame
#' @param target (character) name of target variable
#' @param config Configuration object (from calling [get_automl_config()])
#' @param ... additional arguments passed to [get_automl_model()]
#' @param prop_train proportion of rows to be allocated to training
#' @param scale_numeric_features (logical) whether to scale numeric features
#' (see [scale_numeric_features_in_train_and_test()])
#' @param add_confidence_level (logical) whether to add confidence levels
#' alongside predictions in the test set (default \code{FALSE})
#' @param plot_and_print_variable_importances (logical) whether to plot and print
#' variable importances if they are available (default \code{TRUE})
#' @param nthreads (character) how many threads (cores) to dedicate to the
#' H2O cluster ("half", "minus1", or "all"; see [init_h2o()])
#' @param shutdown_h2o (logical) whether to shut down cluster at the end of the pipeline
#' (default \code{FALSE})
#'
#' @return list with elements:
#' \describe{
#' \item{tt}{train-test split constructed from \code{df}, with predictions and
#' possibly confidence levels added to the test set}
#' \item{automl}{H2O AutoML model}
#' \item{importances}{Variable importances (if requested)}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' pip <- df |> run_automl_pipeline(target="y1")
#' }
#'
#' @seealso
#' [ttsplit()],
#' [scale_numeric_features_in_train_and_test()],
#' [get_automl_model()],
#' [add_predictions_from_automl()]
#'
#' @concept machine_learning
run_automl_pipeline <- function(df,
                                target,
                                config=get_automl_config(),
                                ...,
                                prop_train=.7,
                                scale_numeric_features=FALSE,
                                add_confidence_level=FALSE,
                                plot_and_print_variable_importances=TRUE,
                                nthreads="half",
                                shutdown_h2o=FALSE)
{
  # data prep for h2o
  df <- df |>
    remove_non_ascii_from_df() |>
    cast("character", "factor") |>
    remove_blank_factor_levels()

  # tt and h2o
  tt <- df |> ttsplit(prop_train)
  if(scale_numeric_features) tt <- tt |> scale_numeric_features_in_train_and_test()
  init_h2o(nthreads = nthreads)
  tt_h2o <- ship_train_and_test_to_h2o(tt)
  automl <- get_automl_model(tt_h2o=tt_h2o, target=target, config=config, ...)
  tt <- add_predictions_from_automl(automl,
                                    tt,
                                    tt_h2o,
                                    add_confidence_level = add_confidence_level)
  pip <- list(tt=tt, automl=automl)
  if(plot_and_print_variable_importances) pip$importances <- automl |> plot_and_print_variable_importances()
  if(shutdown_h2o) h2o.shutdown(F)

  # out
  pip

}
