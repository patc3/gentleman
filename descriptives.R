message("Gentleman: Descriptives")

#### helper fn ####
# helper fn: format p values
format_p <- function(p)
{
  "
  input: (named) vector of p-values (numeric)
  output: (named) vector of formatted p-values (character)
  "
  
  p |> 
    sprintf(fmt="%.3f") |> # keep 3 decimals
    sub(pattern = "^0.", replacement = "\\.") |> # remove leading 0
    replace_in_vector(find=".000", replace="< .001") |> # replace .000 with < .001
    replace_in_vector(find="NA", replace="") |> 
    setNames(p |> names())
}

# helper fn: add group key to column names as suffix
add_group_key_to_desc_tbl <- function(tbl, group_key)
{
  "
  input: tbl from some tbl_fn; group_key from dplyr::group_map()
  return: tbl with column names changed to include group suffix (group name and key)
  "
  # add group key to column names
  cols <- colnames(tbl) != "Var"
  colnames(tbl)[cols] <- colnames(tbl)[cols] |> 
    paste0(" (", names(group_key) |> paste0(collapse="."), 
           " #", group_key[1,] |> unlist() |> paste0(collapse="."), ")")
  
  # out
  return(tbl)
}

# helper fn: pivot
to_long <- function(df, repeated_vars)
{
  "
  input: repeated_vars is named list of var pairs
  output: dflong
  "
  # add id row
  df$id <- 1:nrow(df) |> factor()
  
  #
  dflong <- repeated_vars |> lapply(
    \(repeated_var)
    {
      # pivot
      dflong <- df["id" |> c(repeated_var)] |> 
        pivot_longer(cols=all_of(repeated_var), names_to="Time") |> 
        as.data.frame()
      for(t in 1:length(repeated_var)) dflong$Time <- dflong$Time |> replace_in_vector(find=repeated_var[t], replace=t)
      return(dflong)
    }
  )
  
  # col name
  for(i in 1:length(dflong)) colnames(dflong[[i]])[which(colnames(dflong[[i]])=="value")] <- names(repeated_vars)[i]
  
  # merge
  dflong <- dflong |> Reduce(f = \(df1, df2)merge(df1, df2, by=c("id", "Time"), sort=FALSE))
  
  # out
  return(dflong)
}



#### numeric ####

# internal, generic function to get summary table for numeric vars from a df
tbl_fn_num <- function(df, vars)
{
  "
  use as tbl_fn in get_desc_table() for numeric vars
  return: df with columns Var, Mean (SD) for each `vars`
  "
  
  # helper fn
  format_mean_sd <- function(tbl)
  {
    tbl |> 
      mutate(across(c(mean, sd), ~sprintf('%.2f',.) )) |> 
      mutate(`Mean (SD)`=paste0(mean, " (", sd, ")") |> 
               replace_in_vector("NaN (NA)", "")) |> 
      select(-c(mean, sd))
  }
  
  # functions
  desc_fns <- list(
    `n`=~sum(!is.na(.)),
    `mean`=~mean(., na.rm=T), 
    `sd`=~sd(., na.rm=T)
  )
  
  # summarize
  tbl <- df |> 
    select(all_of(vars)) |> 
    summarize(across(everything(), .fns=desc_fns, .names="{.col}---{.fn}")) |>
    pivot_longer(cols=everything(), names_to = c("Var", ".value"), names_sep="---") |> 
    as.data.frame() |> 
    format_mean_sd()
  
  # out
  return(tbl)
}
get_desc_num_summary_table <- tbl_fn_num # for backwards compatibility


# get sig. test for difference between groups on numeric variables
ana_fn_aov <- function(df, vars, group) 
{
  "
  use as ana_fn in get_desc_table() for numeric vars
  return: named list of p-values (ANOVA: vars ~ group)
  "
  
  ana <- vars |> lapply(
    \(v) tryCatch({
      aov("`" |> paste0(v, "` ~ `", group, "`") |> as.formula(), data=df) |> 
        summary() |> 
        {\(s)s[[1]]$`Pr(>F)`[1]}() |> 
        format_p()
    }, error=\(e) return(NA))
  ) |> setNames(vars) |> 
    make_df_from_named_list(index="Var", value="p")
  
  # out
  return(ana)
}

# ana fn: RM ANOVA
ana_fn_rm_aov <- function(df, vars, group, id, add_cohen=FALSE) 
{
  "
  use as ana_fn in get_desc_table() for numeric vars
  df is in long format
  group is time variable
  return: named list of p-values (RM-ANOVA: vars ~ group)
  "
  
  ana <- vars |> lapply(
    \(v) tryCatch({
      aov("`" |> paste0(v, "` ~ `", group, "`") |> paste0("+ Error(`", id, "`)") |> as.formula(), data=df) |> 
        broom::tidy() |> 
        filter(stratum=="Within", term==group) |> 
        pull(p.value)
    }, error=\(e) return(NA))
  ) |> setNames(vars) |> 
    make_df_from_named_list(index="Var", value="p")
  
  # effect size
  if(add_cohen)
  {
    # groups
    grps <- df[,group] |> unique()
    
    # get cohen
    get_cohen <- function(var)
    {
      df_wide <- df |> 
        select(all_of(c(id, group, var))) |> 
        pivot_wider(id_cols=all_of(id), names_from=all_of(group), values_from=all_of(var)) |> 
        select(-all_of(id)) |> 
        as.data.frame()
      diff <- df_wide[,2] - df_wide[,1]
      mean_diff <- mean(diff, na.rm=T)
      cohen <- mean_diff/sd(diff, na.rm=T)
      cohen <- cohen |> weights::rd(2)# or sprintf(fmt="%.2f") |> sub(pattern = "^(-?)0.", replacement = "\\1.")
      mean_diff <- mean_diff |> sprintf(fmt="%.2f")
      c(Diff=mean_diff, Cohen=cohen)
    }
    
    cohen <- vars |> 
      lapply(get_cohen) |> 
      setNames(vars) |> 
      make_df_from_named_list("Var", value=c("Diff", "Cohen"))

    # format diff with stars
    cohen$Diff <- cohen$Diff |> paste0(weights::starmaker(ana$p))
    
    # merge with p
    ana <- ana |> left_join(cohen, by="Var")
  }
  
  # format p
  ana <- ana |> mutate(p=p |> format_p())
  
  # out
  return(ana)
}


#### categorical ####

# internal, generic function to get summary table for categorical vars from a df
tbl_fn_fac <- function(df, vars)
{
  "
  use as tbl_fn in get_desc_table() for categorical vars
  return: df with columns Var, N, % for each category of `vars`
  "
  
  # helper fn
  get_desc_fac_n_or_prop <- function(df, vars, prop=FALSE)
  {
    # helper fn
    #get_table_for_column <- \(c) table(c, useNA="always")
    
    # do
    df |> 
      select(all_of(vars)) |> 
      {\(d) if(!prop) lapply(d, table, useNA="always") 
        else lapply(d, \(c) (.t <- c |> table(useNA="no"))/sum(.t)) }() |> 
      unlist() |> 
      as.data.frame() |> 
      {\(d) 
        if(prop) d |> rename(`%`=1) |> mutate(`%`=sprintf("%.0f%%", `%`*100)) # format 
        else d |> rename(N=1)}() |> 
      rownames_to_column("Var")
  }
  
  # do
  tbl <- get_desc_fac_n_or_prop(df, vars) |> 
    left_join(get_desc_fac_n_or_prop(df, vars, prop=T), by="Var")
  
  # add variable name before listing categories
  for (v in vars)
  {
    # alternative to dplyr::add_row(): 
    # https://stackoverflow.com/questions/11561856/add-new-row-to-dataframe-at-specific-row-index-not-appended
    i_row <- grep(v |> paste0("\\."), tbl$Var)[1] # .group is suffix added by table()
    tbl <- tbl |> add_row(Var=v, .before=i_row)
    tbl[i_row,] <- tbl[i_row,] |> unlist()# |> replace_in_vector(NA, "")
  }
  
  # replace NA with blank (for NA lines and rows with variable names)
  tbl[is.na(tbl)] <- ""
  
  # out
  return(tbl)
}
get_desc_fac_n_and_prop <- tbl_fn_fac # for backwards compatibility


# get sig. test for difference between groups on factor (categorical) variables
ana_fn_chisq <- function(df, vars, group)
{
  "
  use as ana_fn in get_desc_table() for categorical vars
  return: named list of p-values (Chi square: vars ~ group)
  "
  
  ana <- vars |> lapply(
    \(v) tryCatch({
      chisq.test(x=df[,v] |> factor(), y=df[,group] |> factor(), correct=FALSE)$p.value |> 
        format_p()
    }, error=\(e) return(NA))
  ) |> setNames(vars) |> 
    make_df_from_named_list(index="Var", value="p")
  
  # out
  return(ana)
}


#### table ####

# get a descriptive table for either num or fac vars
get_desc_table <- function(df, vars, tbl_fn, group=NULL, ana_fn=NULL, ...)
{
  "
  input:
    df is dataframe
    vars is vector of variables
    tbl_fn is fn with (df, vars, group_key=NULL) arguments, returns df with column Var
    group is name of grouping variable in df
    ana_fn is fn with (df, vars, group) arguments, returns named list of values (match with Var)
    ... passed to ana_fn
  return: df in format returned by tbl_fn
  "
  
  #### global ####
  tbl <- df |> tbl_fn(vars)
  
  
  #### by group ####
  if(!is.null(group))
  {
    tbl_grp <- df |> 
      group_by(across(all_of(group))) |> 
      group_map(.f=~tbl_fn(.x,vars) |> add_group_key_to_desc_tbl(.y)) |> 
      Reduce(f=\(df1,df2)full_join(df1,df2,by="Var"))
    
    # analysis values
    if(!is.null(ana_fn))
    {
      ana <- ana_fn(df, vars, group, ...)
      tbl_grp <- tbl_grp |> left_join(ana, by="Var")
    }
    
    # combine
    tbl <- tbl |> 
      left_join(tbl_grp, by="Var") |> 
      mutate(across(all_of(tbl_grp |> names()), ~replace_na(as.character(.x), "")))
  }
  
  
  #### out ####
  return(tbl)
}




# this is a wrapper, with group "Time" and id arg passed to ana_fn
# to long, then send var list to get_desc_table
# pass id=idvar to ana_fn via get_desc_table
# ana fn is aov with Error(id)
get_desc_time <- function(df, repeated_vars, ...)
{
  # pivot
  dflong <- df |> to_long(repeated_vars)
  
  # do
  tbl <- get_desc_table(dflong, 
                        vars=names(repeated_vars), 
                        tbl_fn = get_desc_num_summary_table, 
                        group="Time", 
                        ana_fn = ana_fn_rm_aov, 
                        id="id", 
                        ...)
  
  # out
  return(tbl)
  
}


#### homogeneous subsets ####
# get differences between subscales
compare_pairs_of_vars <- function(df, vars, order_output=TRUE)
{
  "
  input: df (filtered or not)
  output: tbl with homogeneous subsets given Bonferroni correction
  "
  
  v_BEACH <- vars
  df <- df |> select({{v_BEACH}})
  pairs <- do.call("expand.grid", list(c(0,1)) |> rep(length(v_BEACH)) |> list()) |> 
    setNames(v_BEACH)
  pairs <- pairs[rowSums(pairs)==2,]
  
  # t tests
  ana <- list()
  p <- data.frame(matrix(1, nrow=length(v_BEACH), ncol=length(v_BEACH)))
  colnames(p) <- v_BEACH; rownames(p) <- v_BEACH
  for (i in 1:nrow(pairs))
  {
    v <- pairs[i,] |> as.logical() |> which() |> {\(ix)v_BEACH[ix]}()
    .p <- t.test(df[,v[1]], df[,v[2]], paired=TRUE, var.equal=TRUE, na.action=na.pass)$p.value
    p[v[1], v[2]] <- .p
    p[v[2], v[1]] <- .p
  }
  ana$p_uncorrected <- p
  
  # significant? as vector
  n_tests <- nrow(pairs)
  alpha <- (.05/n_tests)
  p[,] <- ifelse(p>alpha, "MMGRP", "")
  print("Bonferroni correction for" |> paste(n_tests, "tests"))
  
  # order p rows and columns by mean value
  if(order_output)
  {
    mean_order <- df |> colMeans(na.rm = T) |> order()
    p <- p[mean_order, mean_order]
  }
  ana$subsets <- p
  
  # out
  return(ana)
  
}

