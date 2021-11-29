message("Gentleman: Data Prep")

#### scale & combine ####
# generic fn: combine numeric vars into one on same scale (z)
scale_and_combine <- function(df, vars, name, scale=TRUE)
{
  "
  Scales all vars individually then combines one at a time (from left to right in vars)
  
  input:  df, vars is vector of variable names, name is new variable name
  output: df with var added
  "
  
  # scale
  if(scale) df <- df |> mutate(across({{vars}}, ~as.numeric(scale(.))))
  
  # combine
  df[,name] <- df[,vars[1]]
  for(i in 2:length(vars)) df[,name] <- is.na(df[,name]) |> ifelse(df[,vars[i]], df[,name])
  
  # out
  print("Added var" |> paste(name))
  return(df)
}


#### factors ####

# effect coding
make_factors_into_effect_codes <- function(df, factors, remove_selected_columns = TRUE, ignore_na = TRUE, change_which_dummy_is_removed=F)
{
  "
  input: df
  this selects categorical predictors (factors), then makes a sub-df with the factor only (and temp DELETE column)
  uses cbind to merge
  argument remove_first_dummy used in this function, ignore_na in fastDummies
  
  out: df with effect codes (and original var removed)
  "
  
  codes <- list()
  for (v in factors)
  {
    .df <- df |> select({{v}})
    .df[,v] <- .df[,v] |> as.factor() # for fastDummies
    
    # change order of factor levels
    if(change_which_dummy_is_removed)
    {
      l <- levels(.df[,v])
      new_l <- l
      while(new_l[1] == l[1]) new_l <- sample(l, length(l), replace = F)
      .df[,v] <- factor(.df[,v], levels=new_l)
    }
    
    # add temp var so that dummy_cols works properly
    .df$DELETE <- NA
    codes[[v]] <- fastDummies::dummy_cols(.df, remove_first_dummy = TRUE, ignore_na = ignore_na, remove_selected_columns = TRUE)
    codes[[v]] <- codes[[v]] |> select(-DELETE)
  }
  
  # change 0 rows to -1 (dummy -> deviation effect)
  for (i in 1:length(codes))
  {
    i_0s <- rowSums(codes[[i]], na.rm = T) == 0 & rowSums(is.na(codes[[i]])) != ncol(codes[[i]])
    codes[[i]][i_0s,] <- -1 # make this 0 and compare with fn for dummies, make sure it's exactly same results
  }
  
  # remove original variables if requested
  if(remove_selected_columns) df <- df[,!names(df) %in% factors]

  # cbind
  codes_names <- codes |> lapply(names) |> unlist()
  codes <- do.call(cbind, codes)
  colnames(codes) <- codes_names
  df <- df |> cbind(codes)

  
  # out
  print("First, used fastDummies to make factors into dummies, removing the first dummy")
  print("Second, replaced 0s with -1s for rows with all 0s")
  print("Replaced factor names with effect codes in VARS")
  return(df)
  
}

# group some factor levels
group_some_factor_levels <- function(df, map)
{
  '
  inputs are all factors (make into factors before running)
  map is named list: names are var names; values are lists with vectors of factor levels to combine
  e.g.:
  
  map <- list( # THIS USES THE FIRST VALUE AS THE RESULTING GROUP FOR ALL SUBSEQUENT VALUES IN VECTOR
    occMereV=list(c("1", "2", "3"), c("4", "5", "6", "7")) # factor levels will be 1 and 4
    ,occPereV=list(c("1", "2", "3"), c("4", "5", "6", "7"))
    ,dxFratrieVCMF=list(c(1:4 |> as.character()))
    ,diagnosV=list(c("1", "3", "5", "6"))
  )
  '

  for (iv in 1:length(map))
  {
    v <- names(map)[iv]
    df[,v] <- as.character(df[,v]) # to avoid using nonexistent levels or be left with levels with 0 rows
    
    for (ig in 1:length(map[[iv]]))
    {
      values <- map[[iv]][[ig]]
      i <- which(df[,v] %in% values)
      df[i,v] <- values[1] # give first value
    }
    
    df[,v] <- as.factor(df[,v])
  }
  
  # out
  print("Grouped factor levels")
  print("Used map to make all values the same (first one of each map)")
  print(map)
  return(df)
  
}