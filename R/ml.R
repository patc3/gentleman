#### helpers ####
#' Remove non-ASCII characters from data.frame
#'
#' This function removes all non-ASCII characters from a data.frame.
#' This is particularly useful in non-English locales when special characters
#' pose problems (e.g. in using the machine learning H2O library).
#'
#' @param df data.frame
#'
#' @return \code{df} with non-ASCII characters removed
#' @export
#'
#' @examples
#' df <- df |> remove_non_ascii_from_df()
#'
#' @seealso
#' [base::iconv()]
remove_non_ascii_from_df <- function(df) # helper function
{
  df_ascii <- df
  for (c in names(df))
  {
    df_ascii[,c] <- iconv(df[,c], "latin1", "ASCII", sub="")
    if(is.numeric(df[,c])) df_ascii[,c] <- as.numeric(df_ascii[,c]) else if (is.factor(df[,c])) df_ascii[,c] <- as.factor(df_ascii[,c])
  }
  names(df_ascii) <- iconv(names(df_ascii), "latin1", "ASCII", sub="")

  # out
  print("Removed non-ascii characters from column names and entire df")

  # return
  return(df_ascii)
}



#' Remove blank factor levels
#'
#' This function removes blank factor levels across a data.frame by
#' replacing blank levels by \code{NA} (missing).
#'
#' @param df data.frame
#'
#' @return \code{df} with factors with blank levels replaced with \code{NA} (missing)
#' @export
#'
#' @examples
#' df <- df |> remove_blank_factor_levels()
remove_blank_factor_levels <- function(df)
{
  for (c in colnames(df))
  {
    if (class(df[,c]) == "factor")
    {
      levels(df[,c])[which(levels(df[,c]) %in% c("", " ", "  "))] <- NA
    }
  }
  print("Replaced factor levels '', ' ', and '  ' to NA")
  return(df)
}




#### config ####
#' Create configuration object for running autoML
#'
#' This function creates the configuration object
#' required to execute [get_automl_model()] and
#' [run_automl_pipeline()].
#'
#' @param target (character) name of the target variable
#' @param nthreads number of processors for use by H2O
#' @param algos (character) vector of included algorithms (see [h2o::h2o.automl()])
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
#' config <- get_automl_config(target="y1")
#' pip <- df |> run_automl_pipeline(config)
#' }
#'
#' @seealso
#' [get_automl_model()], [run_automl_pipeline()], [h2o::h2o.automl()]
get_automl_config <- function(target,
                           nthreads=c("half", "minus1", "all"),
                           algos="DRF",
                           max_runtime_secs=NULL,
                           max_models=1,
                           max_runtime_secs_per_model=60*5,
                           balance_classes=FALSE)
{
  # args
  nthreads <- match.arg(nthreads)

  # create config
  config <- list(
    target=target,
    automl=list(
      nthreads=nthreads
      ,algos=algos
      ,max_runtime_secs = max_runtime_secs
      ,max_models=max_models
      ,max_runtime_secs_per_model=max_runtime_secs_per_model
      ,balance_classes=balance_classes
    )
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
#' [replace_target_in_test_set_with_missing_and_add_ref_table_to_environment()],
#' [add_target_back_to_test_set_from_ref_table()]
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
scale_numeric_features_in_train_and_test <- function(tt)
{
  # scale numeric features train & test
  v_num <- colnames(tt$train)[which(sapply(tt$train, class) %in% c("numeric", "integer"))]
  scales <- dataPreparation::build_scales(data_set = tt$train, cols=v_num, verbose=TRUE)
  tt$train <- dataPreparation::fast_scale(data_set = tt$train, scales = scales, verbose = TRUE) |> as.data.frame()

  # test
  tt$test <- dataPreparation::fast_scale(data_set = tt$test, scales=scales, verbose=TRUE) |> as.data.frame

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
#'    mutate(id=1:nrow(df)) |>
#'    ttsplit() |>
#'    replace_target_in_test_set_with_missing_and_add_ref_table_to_environment("y1", "id", "target_values")
#'
#' @seealso
#' [add_target_back_to_test_set_from_ref_table()]
replace_target_in_test_set_with_missing_and_add_ref_table_to_environment <- function(tt, target, unique_id, ref_name)
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
#' Title
#'
#' @param tt
#' @param ref_table
#'
#' @return
#' @export
#'
#' @examples
#'
#' @seealso
#' [replace_target_in_test_set_with_missing_and_add_ref_table_to_environment()]
add_target_back_to_test_set_from_ref_table <- function(tt, ref_table)
{
  "
  input: tt is train-test list; ref_table is reference table with id and target column created with replace_target_in_test...()
  output: tt list with target in train replaced with actual values
  "
  id_var <- names(ref_table)[1]
  target_var <- names(ref_table)[2]
  tt$test[,target_var] <- ref_table[,target_var][match(x=tt$test[,id_var], table=ref_table[,id_var])]
  print(paste0("Replaced values in target '", target_var, "' with values from reference table"))
  return(tt)
}





#### h2o ####
#' Title
#'
#' @param nthreads
#'
#' @return
#' @export
#'
#' @examples
#'
#' @seealso
init_h2o <- function(nthreads=c("minus1", "half", "all"))
{
  "
  input: nthreads
  init h2o cluster (no output)
  "
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



#' Title
#'
#' @param tt
#'
#' @return
#' @export
#'
#' @examples
#'
#' @seealso
ship_train_and_test_to_h2o <- function(tt)
{
  "
  input: tt list (train and  test)
  output: h2o pointers
  "

  # train-test
  train <- as.h2o(tt$train, "train")
  test <- as.h2o(tt$test, "test")
  print("Destination frames added: 'train' and 'test'")

  # check
  print("Row check:")
  print(paste0("H2O dfs: train = ", nrow(as.data.frame(train)), ", test = ", nrow(as.data.frame(test))))
  print(paste0("R dfs: "))
  print(lapply(tt, nrow))

  # out
  return(list(train=train, test=test))

}



# automl
#' Title
#'
#' @param config
#' @param tt_h2o
#'
#' @return
#' @export
#'
#' @examples
#'
#' @seealso
get_automl_model <- function(config, tt_h2o)
{
  "
  input: tt_h2o is list of h2o pointers, but only $train is used
  return automl object
  "

  # model
  automl <- h2o.automl(
    y=config$target,
    training_frame = tt_h2o$train,
    stopping_tolerance = .01,
    include_algos = config$automl$algos,
    keep_cross_validation_predictions = FALSE,
    keep_cross_validation_models = FALSE,
    max_runtime_secs = config$automl$max_runtime_secs
    ,max_models = config$automl$max_models
    ,max_runtime_secs_per_model = config$automl$max_runtime_secs_per_model
    ,balance_classes=config$automl$balance_classes
  )

  # out
  return(automl)
}



# add predictions
#' Title
#'
#' @param automl
#' @param tt
#' @param tt_h2o
#' @param add_confidence_level
#'
#' @return
#' @export
#'
#' @examples
#'
#' @seealso
add_predictions_from_automl <- function(automl, tt, tt_h2o, add_confidence_level=TRUE)
{
  "
  input: tt and tt_h2o only $test is used
  return tt with added column to test and confidence level (if requested)
  "

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
#' Title
#'
#' @param automl
#'
#' @return
#' @export
#'
#' @examples
#'
#' @seealso
plot_and_print_variable_importances <- function(automl)
{
  try({
    h2o.varimp_plot(automl@leader)
    print(as.data.frame(h2o.varimp(automl@leader)))
  }, silent=F)
}



#### pipeline ####
#' Title
#'
#' @param df
#' @param config
#' @param prop_train
#' @param scale_numeric_features
#' @param add_confidence_level
#' @param plot_and_print_variable_importances
#' @param shutdown_h2o
#'
#' @return
#' @export
#'
#' @examples
#'
#' @seealso
run_automl_pipeline <- function(df,
                                config,
                                prop_train=.7,
                                scale_numeric_features=FALSE,
                                add_confidence_level=FALSE,
                                plot_and_print_variable_importances=TRUE,
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
  init_h2o(nthreads = config$automl$nthreads)
  tt_h2o <- ship_train_and_test_to_h2o(tt)
  automl <- get_automl_model(config, tt_h2o)
  tt <- add_predictions_from_automl(automl,
                                    tt,
                                    tt_h2o,
                                    add_confidence_level = add_confidence_level)
  if(plot_and_print_variable_importances) automl |> plot_and_print_variable_importances()
  if(shutdown_h2o) h2o.shutdown(F)

  # out
  list(tt=tt, automl=automl)

}
