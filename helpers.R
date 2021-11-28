message("Gentleman: Helpers")

#### alias ####
view <- utils::View
copy <- function(tbl) write.csv2(tbl,file="clipboard-9999", row.names=F)


#### find & replace ####
# internal fn: replace at position in vec
replace_in_vector_at_position <- function(vec, position, replace)
{
  # add (replace)
  vec <- c(vec[1:position], 
           replace,
           if(position < length(vec)) vec[(position+1):length(vec)] else NULL)
  
  # remove
  vec <- vec[-position]
  
  # out
  return(vec)
}

# internal fn: replace all occurrences of find with replace in vec
replace_in_vector <- function(vec, find, replace, n_max=Inf)
{
  n <- sum(vec %in% find)
  if(n == 0) return(vec) else n <- min(n, n_max) # no match, return vec
  
  # replace
  for(it in 1:n) 
  {
    position <- which(vec %in% find)[1]
    vec <- replace_in_vector_at_position(vec, position, replace)
  }
  
  # out
  return(vec)
}

# generic user-facing fn: replaces all find values with replace in (possibly nested) list
replace_in_nested_list <- function(list, find, replace, n_max_per_vector=Inf)
{
  if(find |> identical(replace)) return(list)
  if(n_max_per_vector <= 0) stop("n_max_per_vector must be >= 1")
  new_list <- list |> rapply(f=\(v)replace_in_vector(v, find=find, replace=replace, n_max=n_max_per_vector), how="list")
  if(new_list |> identical(list)) warning("Searched value not found in list.")
  return(new_list)
}


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