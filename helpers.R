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



#### lists ####
# helper fn: make named list into dataframe
make_df_from_named_list <- function(list, index="Var", value="Value" |> paste0(1:length(list[[1]])))
{
  "
  input: named list
  return: data.frame
  "
  list |>
    do.call(what="rbind") |> 
    as.data.frame() |> 
    rownames_to_column() |> 
    setNames(c(index, value))
}


#### pairs ####
# get all possible pairs from vector
# return matrix with all possible pairs in each row
get_all_pairs <- function(vec, direction=c("direct", "double", "reverse"))
{
  "
  vec is vector of vars
  direction is permutation, combination, or reverse
  out: matrix
  "
  
  direction <- match.arg(direction)
  pairs <- do.call("expand.grid", list(c(F,T)) |> rep(length(vec)) |> list())
  pairs <- pairs[rowSums(pairs)==2,]
  all_pairs <- apply(pairs, 1, \(r) vec[r |> as.logical()]) |> t()
  if(direction=="double") all_pairs <- all_pairs |> rbind(all_pairs[,c(2,1)])
  if(direction=="reverse") all_pairs <- all_pairs[,c(2,1)]
  return(all_pairs)
}