
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
                                config,
                                add_confidence_level=FALSE,
                                plot_and_print_variable_importances=TRUE,
                                shutdown_h2o=FALSE)
{
  tt <- df |> ttsplit(prop_train)
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
