#### alias ####

#' Alias for utils::View()
#'
#' This function is mostly useful as a shortcut to avoid default viewer in RStudio.
#'
#' @param ... Passed to \code{utils::View()}
#'
#' @return Invisible \code{NULL}. See [utils::View()]
#' @export
#'
#' @examples
#' \dontrun{
#' view(df)
#' }
#'
#' @concept helpers
view <- function(...){ utils::View(...) }


#' Wrapper for writing CSV2 to clipboard
#'
#' This is a wrapper for \code{write.csv2()} with \code{file="clipboard-9999"}.
#' Ideal for copy-pasting a data.frame into Excel in non-English locale.
#'
#' @param tbl Object to be written
#'
#' @export
#'
#' @examples
#' df |> copy()
#'
#' @concept helpers
copy <- function(tbl){ write.csv2(tbl,file="clipboard-9999", row.names=F) }


#' Writes table to text file in current directory
#'
#' This is a wrapper for \code{write.table()} with \code{file="gentleman_out.txt"}.
#'
#' @param tbl Object to be written
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df |> copy2()
#' }
#'
#' @concept helpers
copy2 <- function(tbl){ write.table(tbl, file="gentleman_out.txt", sep=";", col.names=T, row.names=F, append=T) }



#' Wrapper for writing table to clipboard
#'
#' This is a wrapper for \code{write.table()} with \code{file="clipboard-9999"}
#' and `quote=FALSE`. Ideal for copy-pasting a data.frame into a text file.
#'
#' @param tbl Object to be written
#'
#' @export
#'
#' @examples
#' df |> copy3()
#'
#' @concept helpers
copy3 <- function(tbl){ write.table(tbl,
                                    file="clipboard-9999",
                                    quote = FALSE,
                                    col.names=TRUE,
                                    row.names=FALSE) }


#### operators ####

#' Pasting operators
#'
#' These operators are used to paste objects together as you would
#' with the `+` operator in Python.
#'
#' @param lhs left-hand side
#' @param rhs right-hand side
#'
#' @return (character) pasted objects
#' @rdname paste
#' @concept helpers
#' @export
#'
#' @examples
#' "this adds a space" %P% "between strings"
`%P%` <- function(lhs, rhs){ paste(lhs, rhs, sep=" ") }

#' @rdname paste
#' @export
#' @examples
#' "this doesn" %p% "'" %p% "t"
`%p%` <- function(lhs, rhs){ paste(lhs, rhs, sep="") }

#' @rdname paste
#' @export
#' @examples
#' "This skips" %N% "a line" |> cat()
`%N%` <- function(lhs, rhs){ paste(lhs, rhs, sep="\n") }

#' @rdname paste
#' @export
#' @examples
#' "y1" %P% "~" %P% (("x" %p% 1:3) %C% "+") # collapse with space
`%C%` <- function(lhs, rhs){ paste(lhs, collapse=paste("", rhs, "")) }

#' @rdname paste
#' @export
#' @examples
#' "y1" %P% "~" %P% (("x" %p% 1:3) %c% "+") # collapse without space
`%c%` <- function(lhs, rhs){ paste(lhs, collapse=rhs) }




#### find & replace ####
# internal fn: replace at position in vec
#' Replace vector element at given position with value
#'
#' This function replaces the element at a given position of a vector by the replacement value.
#'
#' @param vec Vector
#' @param position Position at which to replace value
#' @param replace Replacement value
#'
#' @return Vector with value replaced
#' @export
#'
#' @examples
#' c(1,2,3,4,5) |> replace_in_vector_at_position(1, 0)
#'
#' @concept helpers
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
#' Replace all occurrences of a value in a vector and replace
#'
#' @param vec Vector
#' @param find Value to replace
#' @param replace Replacement value
#' @param n_max Maximum number of replacements (starting at index 1)
#'
#' @return Vector with values replaced
#' @export
#'
#' @examples
#' c(1,1,3,6,6,7,9) |> replace_in_vector(find=6, replace=5)
#'
#' @concept helpers
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
#' Replace values in (possibly nested) list of vectors
#'
#' @param list List of vectors
#' @param find Value to replace
#' @param replace Replacement value
#' @param n_max_per_vector Maximum number of replacements per vector
#'
#' @return List with values replaced
#' @export
#'
#' @examples
#' values <- list(
#'   c(1,2,3,4,5),
#'   c(4:10),
#'   list(
#'     1:3,
#'     4:7
#'   )
#' )
#' values |> replace_in_nested_list(find=5, replace=0)
#'
#' @concept helpers
replace_in_nested_list <- function(list, find, replace, n_max_per_vector=Inf)
{
  if(find |> identical(replace)) return(list)
  if(n_max_per_vector <= 0) stop("n_max_per_vector must be >= 1")
  new_list <- list |> rapply(f=\(v)replace_in_vector(v, find=find, replace=replace, n_max=n_max_per_vector), how="list")
  if(new_list |> identical(list)) warning("Searched value not found in list.")
  return(new_list)
}


#' Substring character from the right side
#'
#' This function returns the nth last characters of a string
#'
#' @param x character
#' @param n number of digits to extract starting from the right
#'
#' @return last \code{n}th character of \code{x}
#' @export
#'
#' @examples
#' vars <- c("Gender.x", "Gender.y")
#' vars |> substr_right(1)
#'
#' @concept helpers
substr_right <- function(x, n){
  n_char <- nchar(x)
  substr(x, n_char-n+1, n_char)
}



#### lists ####
# helper fn: make named list into dataframe
#' Combine rows of named list of data.frames
#'
#' This function is an internal, helper function used by \code{ana_fn} functions
#' to combine p-values (and other group statistics) across several variables.
#'
#' @details
#' This function uses [base::rbind()] to combine the data.frames in \code{list},
#' which requires the data.frames to have the same number of columns.
#'
#' @param list Named list of data.frames
#' @param index Variable name to store data.frame names
#' @param value Variable name(s) for value(s) stored in each data.frame (in ana_fn functions, typically a p-value)
#'
#' @return data.frame
#' @export
#'
#' @examples
#' l <- list(
#'    Gender=data.frame(p=.964),
#'    Nationality=data.frame(p=.009)
#' )
#' l |> make_df_from_named_list()
#'
#' @seealso [gentleman::ana_fn_aov()], [gentleman::ana_fn_rm_aov()], [gentleman::ana_fn_chisq()]
#'
#' @concept helpers
make_df_from_named_list <- function(list, index="Var", value="Value" |> paste0(1:length(list[[1]])))
{
  "
  input: named list
  return: data.frame
  "
  list |>
    do.call(what="rbind") |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    setNames(c(index, value))
}


#### pairs ####
# get all possible pairs from vector
# return matrix with all possible pairs in each row
#' Get all possible pairs from vector
#'
#' This function returns a matrix of all pairs of values in a vector.
#'
#' @param vec Vector
#' @param direction \itemize{
#' \item{\code{direct} (default)}
#' {Like permutation: (a,b) and (b,a) are treated as the same pair and only one is returned}
#' \item{\code{double}}
#' {Like combination: (a,b) and (b,a) are treated as distinct pairs}
#' \item{\code{reverse}}
#' {Like direct, but the two columns are reversed}
#'
#' }
#'
#' @return Two-column matrix of pairs
#' @export
#'
#' @examples
#' get_all_pairs(c("x1", "x2", "y1", "y2"))
#'
#' @concept helpers
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


# helper fn to generate pairs of vars with operator from vars list
#' Helper function to generate pairs with an operator separator
#'
#' Used for generating formulas in lavaan models.
#'
#' @param vars List of variables
#' @param op Operator (typically a lavaan operator like \code{~})
#' @param ... Named arguments passed to \code{get_all_pairs()}
#'
#' @return Character vector with all pairs with operator separator
#'
#' @concept helpers
get_all_pairs_with_op <- function(vars, op, ...)
{
  all_pairs <- vars |>
    do.call(what="rbind") |>
    apply(2, get_all_pairs, ..., simplify=F)

  all_pairs |>
    lapply(\(p) apply(p, 1, paste, collapse=op) |> paste(collapse="\n")) |>
    unlist() |>
    paste(collapse="\n")
}
