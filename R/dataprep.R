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
  
  # original df, to which new var will be added
  df_orig <- df
  
  # make factor vars into chars
  for(v in vars) if(df[,v] |> is.factor()) df[,v] <- df[,v] |> as.character()
  
  # scale
  if(scale) df <- df |> mutate(across({{vars}}, ~as.numeric(scale(.))))
  
  # combine
  combined_var <- df[,vars[1]]
  for(i in 2:length(vars)) combined_var <- is.na(combined_var) |> ifelse(df[,vars[i]], combined_var)
  
  # add to original df
  df_orig[,name] <- combined_var
  
  # out
  print("Added var" |> paste(name))
  return(df_orig)
}


#### factors ####

# contr. fn: effect coding when k>2; dummy code when k=2
# use with:
# options(contrasts=c(unordered="contr.dummy_or_effect", ordered="contr.poly"))
contr.dummy_or_effect <- function(...)
{
  contr <- contr.treatment(...)
  
  # effect code if more than 2 groups; otherwise leave as dummy
  if(ncol(contr) > 1)
  {  
    i_0 <- (rowSums(contr)==0) |> which()
    contr[i_0,] <- -1
  }
  
  # out
  contr
}


# effect coding
make_factors_into_effect_codes <- function(df, 
                                           factors, 
                                           remove_selected_columns = TRUE, 
                                           ignore_na = TRUE, 
                                           change_which_dummy_is_removed=F,
                                           make_into_dummy_instead = c())
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
    codes[[v]] <- fastDummies::dummy_cols(.df, 
                                          remove_first_dummy = TRUE, 
                                          ignore_na = ignore_na, 
                                          remove_selected_columns = TRUE)
    codes[[v]] <- codes[[v]] |> select(-DELETE)
  }
  
  # change 0 rows to -1 (dummy -> deviation effect)
  effect_coded <- factors |> setdiff(make_into_dummy_instead)
  for (factor_effect in effect_coded)
  {
    i_0s <- rowSums(codes[[factor_effect]], na.rm = T) == 0 & 
      rowSums(is.na(codes[[factor_effect]])) != ncol(codes[[factor_effect]])
    codes[[factor_effect]][i_0s,] <- -1 # make this 0 and compare with fn for dummies, make sure it's exactly same results
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
  print("Effect coded:"); print(effect_coded)
  print("Dummy coded:"); print(make_into_dummy_instead)
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