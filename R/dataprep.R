#### scale & combine ####
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
#' @param factors Vector of factor names found in \code{df}
#' @param remove_selected_columns (logical) Whether to remove original factors in \code{factors}
#' @param ignore_na (logical) Whether to ignore NA. If TRUE, NA will be treated as a factor level.
#' @param change_which_dummy_is_removed (logical) Randomize factor levels prior to creating codes.
#' @param make_into_dummy_instead Vector of variables in \code{factors} for which to obtain dummy codes instead.
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
  if(keep_original_when_no_match)
  {
    i_NA <- new_values |> is.na() |> which()
    if(length(i_NA)>0)
    {
      new_values[i_NA] <- values[i_NA]
      warning("Unchanged values (not found in map):\n" |>
                paste(values[i_NA] |> paste(collapse="\n")), sep="\n")
    }
  }

  # out
  new_values

}
