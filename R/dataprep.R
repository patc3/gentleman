#### cleaning ####
#' Remove non-ASCII characters from data.frame
#'
#' This function removes all non-ASCII characters from a data.frame.
#' This is particularly useful in non-English locales when special characters
#' pose problems (e.g. when using the machine learning library \pkg{h2o}).
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
#'
#' @concept data_prep
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



#' Remove variables with too many missing
#'
#' This function removes variables from a data.frames when their proportion
#' of missing values is too high.
#'
#' @param df data.frame
#' @param pmissing (proportion 0-1) maximum proportion of
#' missingness tolerated in each variable
#'
#' @return \code{df} with variables with too many missing values removed
#' @export
#'
#' @examples
#' df <- df |> remove_vars_with_too_many_missing(pmissing=.80)
#'
#' @concept data_prep
remove_vars_with_too_many_missing <- function(df, pmissing=.90)
{
  orig <- names(df)
  keep <- sapply(df, function(x) !(sum(is.na(x))/length(x) > pmissing))
  df <- subset(df, select=keep)
  print(paste0("Removed variables with more than ", pmissing*100, "% missing:"))
  print(orig |> setdiff(names(df)))
  return(df)
}




#' Remove empty rows or columns
#'
#' This function removes empty rows or columns (all `NA`s) from a data.frame
#'
#' @param df data.frame
#'
#' @return `df` with empty rows or columns removed
#' @export
#'
#' @examples
#' df <- df |> remove_empty_rows()
#'
#' @concept data_prep
#' @rdname remove_empty
remove_empty_rows <- function(df)
{
  remove <- rowSums(is.na(df)) == ncol(df)
  print("Removing" %P% sum(remove) %P% "row(s)")
  df[!remove,]
}



#' @rdname remove_empty
#' @export
#' @examples
#' df <- df |> remove_empty_cols()
remove_empty_cols <- function(df)
{
  remove <- colSums(is.na(df)) == nrow(df)
  print("Removing" %P% sum(remove) %P% "column(s)")
  df[,!remove]
}






#### types & classes ####
#' Cast variables of one type to another
#'
#' @param df data.frame
#' @param type_from original type (a class from [base::inherits()])
#' @param type_to new type (from calling \code{as.*})
#' @param vars (character) vector of variable names to change
#' if they are of type \code{type_from} (if \code{NULL}, use all)
#'
#' @return data.frame with types changed
#' @export
#'
#' @examples
#' \dontrun{
#' df |>
#'    cast("numeric", "factor") |>
#'    get_desc_table(c("Age", "Nationality"), tbl_fn=tbl_fn_fac)
#' }
#'
#' @concept data_prep
cast <- function(df, type_from, type_to, vars=NULL)
{
  if(is.null(vars)) vars <- names(df)
  v <- df[vars] |> sapply(\(x) inherits(x, type_from))
  v <- names(v)[which(v)]
  for(c in v) df[,c] <- do.call(paste0("as.", type_to), list(df[,c]))

  # out
  print(paste0("Made these variables from ", type_from, " to ", type_to, ":"))
  print(v)
  return(df)
}


#### numeric vars ####

# generic fn: combine numeric vars into one on same scale (z)
#' Combine numeric variables into one
#'
#' This function combines vars in \code{vars} (from left to right)
#' possibly scaling each variable individually prior to combining
#'
#' @param df data.frame containing \code{vars}
#' @param vars Vector of variable names to be combined into one
#' @param name Name of the new variable to be added to df
#' @param scale (logical) Whether to scale each variable prior to combining
#'
#' @return df with variable \code{name} added
#' @export
#'
#' @examples
#' df <- df |> scale_and_combine(c("x1", "x2", "x3"), name="x", scale=TRUE)
#'
#' @concept data_prep
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



#' Rescale using min-max normalization
#'
#' This function rescales variable(s) in a data.frame using
#' min-max normalization with arbitrary min and max values.
#'
#' @details
#' Values are rescaled by subtracting the minimum from each value
#' and multiplying by the new range, and dividing by the current
#' range, and adding the new minimum (which acts as an offset).
#' Missing values remain missing. See
#' [Feature scaling](https://en.wikipedia.org/wiki/Feature_scaling#Rescaling_(min-max_normalization))
#' (Wikipedia)
#'
#' @param df data.frame
#' @param vars (character) variables to rescale
#' @param min (numeric) new minimum value (default 0)
#' @param max (numeric) new maximum value (default 1)
#'
#' @return `df` with `vars` rescaled
#' @export
#'
#' @examples
#' df <- df |> rescale_min_max(c("x1", "x2", "x3"))
#'
#' @concept data_prep
rescale_min_max <- function(df, vars, min=0, max=1)
{
  for(v in vars)
  {
    x<-df[,v]
    xp <- min + (x-min(x, na.rm=T))*(max-min)/(max(x, na.rm=T) - min(x, na.rm=T))
    df[,v] <- xp
  }

  # out
  print("Rescaled vars to range from" %P% min %P% "to" %P% max %p% ":")
  print(vars)
  df
}




#' Dichotomize by picking value returned by a function
#'
#' This function dichotomizes (0/1) a variable by applying a function to it
#' and checking whether each element equals the function's return value.
#'
#' @param df data.frame
#' @param vars (character) variable names to dichotomize
#' @param value_fn function that returns desired value for `1` (default `max()`)
#' @param na.rm (logical) whether to remove NAs before applying `value_fn` (default `TRUE`)
#' @param ... (optional) additional arguments passed to `value_fn`
#'
#' @return `df` with `vars` dichotomized
#' @export
#'
#' @examples
#' df <- df |> dichotomize(vars="x1")
#'
#' @concept data_prep
dichotomize <- function(df, vars, value_fn=max, na.rm=TRUE, ...)
{
  print("Making 0-1 numeric by matching to value obtained with value_fn() (1) or else (0)")
  for(var in vars)
  {
    values <- if(na.rm) df[,var] |> na.omit() |> as.numeric() else df[,var]
    df[,var] <- (df[,var]==value_fn(values, ...)) |> as.numeric()
  }
  df
}



#' Winsorize variables
#'
#' This function replaces all values more extreme than a desired
#' minimum and/or maximum value in a variable by that value.
#'
#' @param df data.frame
#' @param var (character) variable to winsorize
#' @param min `var`'s new min value (default `NULL`, keep as is)
#' @param max `var`'s new max value (default `NULL`, keep as is)
#'
#' @return `df` with `var` winsorized
#' @export
#'
#' @examples
#' df <- df |> winsorize("x1", max=1)
#'
#' @concept data_prep
winsorize <- function(df, var, min=NULL, max=NULL)
{
  i_min <- which(df[,var] < min)
  df[i_min,var] <- min

  i_max <- which(df[,var] > max)
  df[i_max,var] <- max

  # out
  print("Replaced" |> paste(length(i_min)+length(i_max), "values in var", var))
  df
}



#### factors ####

# contr. fn: effect coding when k>2; dummy code when k=2
# use with:
# options(contrasts=c(unordered="contr.dummy_or_effect", ordered="contr.poly"))
#' Contrasts for factors using effect coding (when k > 2) or dummy coding (when k = 2)
#'
#' @param ... Passed to \code{contr.treatment()}
#'
#' @return Matrix with contrasts
#' @export
#'
#' @examples
#' contr.dummy_or_effect(3)
#' options(contrasts=c(unordered="contr.dummy_or_effect", ordered="contr.poly"))
#'
#' @concept data_prep
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
#' Generates effect codes for factors
#'
#' This function creates new variables representing effect codes for selected
#' factors. It can also be used to generate dummy codes instead.
#'
#' This function uses the library \code{fastDummies} to generate dummy codes,
#' then recodes the dummy codes into effect codes (except for variables whose name is
#' also specified in \code{make_into_dummy_instead}).
#'
#' @param df A data.frame
#' @param factors Vector of factor names found in `df`
#' @param remove_selected_columns (logical) Whether to remove original factors in `factors` (default `TRUE`)
#' @param ignore_na (logical) Whether to ignore NA. If `FALSE`, NA will be treated as a factor level (default `TRUE`)
#' @param change_which_dummy_is_removed (logical) Randomize factor levels prior to creating codes (default `FALSE`)
#' @param make_into_dummy_instead Vector of variables in `factors` for which to obtain dummy codes instead.
#'
#' @return \code{df} with effect codes added (and original factors possibly removed)
#' @export
#'
#' @examples
#' \dontrun{
#' df |> make_factors_into_effect_codes(
#'   factors=c("Gender", "Nationality"),
#'   make_into_dummy_instead = "Gender"
#' )
#' }
#'
#' @concept data_prep
make_factors_into_effect_codes <- function(df,
                                           factors,
                                           remove_selected_columns = TRUE,
                                           ignore_na = TRUE,
                                           change_which_dummy_is_removed=FALSE,
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
#' Combine some factor levels using a map
#'
#' This function recodes factors by combining some of the original factor levels together.
#'
#' The names in \code{map} correspond to the factor names in \code{df} whose levels need to be combined.
#' Each element in \code{map} is a list with one or more vector(s) of factor levels to combine together,
#' using the first element in each vector as the value for all levels in that vector.
#'
#' @param df data.frame
#' @param map Named list
#'
#' @return \code{df} with factors in \code{names(map)} recoded with combined levels
#' @export
#'
#' @examples
#' \dontrun{
#' map <- list(Nationality=list(c("US", "Puerto Rico"), c("Canada", "Quebec")))
#' df <- df |> group_some_factor_levels(map)
#' }
#'
#' @concept data_prep
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
#'
#' @concept data_prep
remove_blank_factor_levels <- function(df)
{
  for (c in colnames(df))
  {
    if (is.factor(df[,c]))
    {
      levels(df[,c])[which(levels(df[,c]) %in% c("", " ", "  "))] <- NA
    }
  }
  print("Replaced factor levels '', ' ', and '  ' with NA")
  return(df)
}




#' Remove factors with too many levels
#'
#' This function removes from a data.frame factors that have
#' too many levels.
#'
#' @param df data.frame
#' @param maxlevels (integer) maximum number of levels for factors
#'
#' @return \code{df} with factors with more than \code{maxlevels} levels removed
#' @export
#'
#' @examples
#' df <- df |> remove_factors_with_too_many_levels()
#'
#' @concept data_prep
remove_factors_with_too_many_levels <- function(df, maxlevels=20)
{
  orig <- names(df)
  keep <- sapply(df, function(x) !(nlevels(x) > maxlevels))
  df <- subset(df, select=keep)
  print(paste0("Removed factors with more than ", maxlevels, " levels:"))
  print(orig |> setdiff(names(df)))
  return(df)
}





#### recoding & mapping ####
#' Recode values using Excel map (lookup table)
#'
#' This function recodes values using a map (lookup table) located in an Excel sheet.
#'
#' @details
#' The map should have at least two columns, one with original (find) values,
#' one with replacement values. By default the function uses the first
#' sheet in the Excel file specified in \code{path}; it uses the first
#' column as the find column, and the second column as the replacement column;
#' and it assumes the first row is a header (column names).
#' Additional named arguments can be passed to [readxl::read_excel()] via \code{...}
#' for more complicated maps (e.g. custom cell range).
#'
#' If there is at least one value that does not have a match
#' and \code{keep_original_when_no_match} has been requested,
#' a warning will be thrown with the list of values that could not be matched.
#'
#' @param values values to be recoded
#' @param path (character) path to the excel file
#' @param sheet (numeric or character) sheet number or name where the map is
#' @param col_find (numeric or character) column number or name
#' where original values are
#' @param col_replace (numeric or character) column number or name
#' where replacement values are
#' @param first_row_is_header (logical) whether the first row
#' is column names (default \code{TRUE})
#' @param keep_original_when_no_match (logical) whether to retain original
#' value when no match is found (otherwise will be \code{NA}; default \code{FALSE})
#' @param ... additional named arguments passed to [readxl::read_excel()]
#' (like \code{range} or \code{na})
#'
#' @return \code{values} recoded
#' @export
#'
#' @examples
#' \dontrun{
#' df$Nationality <- df$Nationality |>
#'    recode_using_excel_map("map.xlsx", sheet="Nationality")
#' }
#'
#' @concept data_prep
recode_using_excel_map <- function(values,
                                   path,
                                   sheet=1,
                                   col_find=1,
                                   col_replace=2,
                                   first_row_is_header=TRUE,
                                   keep_original_when_no_match=FALSE,
                                   ...)
{
  # map
  map <- readxl::read_excel(path=path,
                            sheet=sheet,
                            col_names=first_row_is_header,
                            ...) |> as.data.frame()
  i <- values |> match(map[,col_find])
  new_values <- map[i,col_replace]

  # in case of NA: retain original name
  if(keep_original_when_no_match && any(is.na(i)))
  {
    i_NA <- i |> is.na() |> which()
    new_values[i_NA] <- values[i_NA]
    warning("Unchanged values (not found in map):\n" |>
              paste(values[i_NA] |> unique() |> paste(collapse="\n")), sep="\n")
  }

  # out
  new_values

}


#' Recode values to NA
#'
#' This function recodes given values to `NA` (missing)
#' in given variables of a data.frame.
#'
#' @param df data.frame
#' @param vars (character) vector of variable names
#' @param values (vector) values to replace with `NA`
#'
#' @return `df` with `values` in `vars` replaced with `NA`
#' @export
#'
#' @examples
#' df <- df |> recode_values_to_NA(vars="x"%p%1:3, values=0)
#'
#' @seealso [recode_using_excel_map()]
#'
#' @concept data_prep
recode_values_to_NA <- function(df, vars, values)
{
  for(v in vars) df[df[,v]%in%values,v] <- NA

  # out
  print("Replaced these values in variables with NA:")
  print(list(Variables=vars, Values=values))
  df
}



#' Reverse-code numeric variables
#'
#' This function reverse-codes numeric variables. For example,
#' a 1-7 Likert scale becomes a 7-1 Likert scale (1=7, 2=6, etc.).
#' This is achieved by taking the maximum value of the variable,
#' adding the minimum (acting as an offset), and subtracting each value.
#'
#' @param df data.frame
#' @param vars (character) vector of variable names
#'
#' @return `df` with `vars` reverse-coded
#' @export
#'
#' @examples
#' df <- df |> reverse_code("x1")
#'
#' @seealso [recode_using_excel_map()]
#'
#' @concept data_prep
reverse_code <- function(df, vars)
{
  for(v in vars)
    df[,v] <- max(df[,v], na.rm=T) + min(df[,v], na.rm=T) - df[,v]

  # out
  print("Reverse-coded these variables:")
  print(vars)
  df
}





#### data.frames ####


# helper fn: pivot
#' Pivot data.frame from wide to long
#'
#' This function pivots a data.frame from wide to long.
#'
#' This is mostly a helper function. For more horsepower, see \code{tidyr::pivot_longer()}.
#'
#' @param df data.frame
#' @param repeated_vars Named list of variables to be pivoted to long format
#'
#' @return \code{df} in long format
#' @export
#'
#' @examples
#' v_repeated <- list(
#'    x=c("x1","x2","x3"),
#'    y=c("y1", "y2", "y3")
#'    )
#' df_long <- df |> to_long(v_repeated)
#'
#' @seealso [tidyr::pivot_longer()]
#'
#' @concept data_prep
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





#' Add variables from one data.frame to another
#'
#' This function adds some variables from one data.frame to another,
#' by matching rows by a given variable.
#'
#' @details
#' This is a convenience function using \code{merge()}, with only selected variables merged
#' into the first data.frame, and with the order of columns preserved. The matching variable
#' may or may not be named the same in both data.frames (as controlled by \code{by_to} and
#' \code{by_from}).
#'
#' @param df_to data.frame in which to add the wanted variables
#' @param df_from data.frame that contains the wanted variables
#' @param by_to (character) matching variable in \code{df_to}
#' @param by_from (character, optional) matching variable in \code{df_from}
#' (if different from \code{by_to})
#' @param vars (character) vector of variable names to add to \code{df_to}
#'
#' @return \code{df_to} with columns added (if any row match by \code{by})
#' @export
#'
#' @examples
#' \dontrun{
#' df <- df |> add_vars_from_one_df_to_another(ref_table, by_to="id", vars=c("Gender", "Age"))
#' }
#'
#' @seealso [base::merge()]
#'
#' @concept data_prep
add_vars_from_one_df_to_another <- function(df_to,
                                            df_from,
                                            by_to,
                                            by_from=NULL,
                                            vars)
{
  if(is.null(by_from)) by <- by_from <- by_to else by <- c(From=by_from, To=by_to)
  .df <- df_from[,c(by_from, vars)]
  df_to <- merge(x=df_to,
                 y=.df,
                 by.x=by_to,
                 by.y=by_from,
                 all.x=TRUE)[, c(names(df_to), vars)] # prevent column reordering

  # out
  print("Added variables:")
  print(vars)
  print("Matching by:")
  print(by)

  # return
  return(df_to)
}


#' Transpose data.frame
#'
#' This function transposes a data.frame by switching its rows and columns.
#'
#' @details
#' If `names_in_first_col` is set to `FALSE`, then the first column
#' in the transposed data.frame will be named `Columns`, indicating original column names,
#' and subsequent columns (original rows) will be named with integers starting at "1".
#'
#' @param df data.frame
#' @param names_in_first_col (logical) whether to use first column
#' as new column names (see Details; default `TRUE`)
#'
#' @return (data.frame) `df` transposed
#' @export
#'
#' @examples
#' df |>
#'    head() |>
#'    transpose_df(FALSE)
#'
#' @concept data_prep
transpose_df <- function(df, names_in_first_col=TRUE)
{
  if(!names_in_first_col) df <- data.frame(Columns=1:nrow(df), df)
  df |>
    t() |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    (\(df){
      names(df) <- df[1,]
      df <- df[-1,]
      df} )()
}
