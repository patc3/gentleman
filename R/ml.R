#### helpers ####
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
ttsplit <- function(df, prop_train=.7)
{
  df_split <- list()
  ix <- sample(nrow(df), round(nrow(df)*prop_train), replace=F)
  df_split$train <- df[ix,]
  df_split$test <- df[-ix,]
  print(paste0("Split df into list with: train, test (proportion train = ", prop_train, ")"))
  return(df_split)
}


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


keep_only_vars_in_both_train_and_test <- function(tt, remove_from_train_only=FALSE)
{
  "
  input: tt is list with train and test
  output: tt list with only same vars in both train and test
  "

  v_in_common <- Reduce(intersect, lapply(tt, names))#set(names(tt$train, tt$test))
  if(!remove_from_train_only) tt <- lapply(tt, function(df) df[,v_in_common]) else tt$train <- tt$train[,v_in_common]
  print(paste0("Selected only variables in common ", ifelse(remove_from_train_only, "in train set (test set unchanged)", "in both train and test sets")))
  return(tt)

}





# prevent target leakage
replace_target_in_test_set_with_missing_and_add_ref_table_to_environment <- function(tt, target, unique_id, ref_name)
{
  "
  input: tt is train-test list; target and unique_id are names of target and unique ID vars; ref_name is name of new object that stores mapping ID and target
  output: tt list with target replaced with NA in test, and new object ref_name in environment
  "
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
plot_and_print_variable_importances <- function(automl)
{
  try({
    h2o.varimp_plot(automl@leader)
    print(as.data.frame(h2o.varimp(automl@leader)))
  }, silent=F)
}



#### pipeline ####
run_automl_pipeline <- function(df,
                                prop_train=.7,
                                scale_numeric_features=FALSE,
                                config,
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
