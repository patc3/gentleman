#' Log output to text file
#'
#' This function outputs (sinks) the console to
#' a text file on disk.
#'
#' @param expr R expression(s) to output to text file
#' @param logger logger object from [get_logger()]
#' (default will create a new logger)
#' @param silent (logical) whether to print file path to
#' console after logging (default \code{TRUE})
#' @param return_logger (logical) whether to return the
#' logger object (useful if no initial logger object was passed)
#'
#' @return Sinks output to text file, and returns logger object if requested
#' @export
#'
#' @examples
#' \dontrun{
#' logtext(print("Hello world!"))
#'
#' logger <- get_logger()
#' logger |> logtext(expr={
#'   print("model without covariates:")
#'   lm(y1 ~ x1, df) |> summary() |> print()
#'   print("model with covariates")
#'   lm(y1 ~ x1 + x2 + x3, df) |> summary() |> print()
#' })
#' }
logtext <- function(expr,
                    logger=get_logger(add_git_info = FALSE,
                                      init_log_file = FALSE),
                    silent=FALSE,
                    return_logger=FALSE)
{
  fpath <- paste0(logger$dir,"/",logger$fname,".txt")
  sink(file=fpath, append=TRUE)
  cat("________________________ beginning output ________________________\n\n")
  expr
  cat("________________________ end output ________________________\n\n\n\n")
  sink()
  if(!silent) print(paste0("Log file at ", fpath))
  if(return_logger) return(logger)
}






#' Retrieve current git commit information
#'
#' This function retrieves the information about the current (latest)
#' git commit information in a specified repository directory.
#'
#' @param dir path to repository directory (default current directory)
#'
#' @return list with elements:
#' \describe{
#' \item{HEAD}{Contents of HEAD}
#' \item{SHA}{Unique commit identifier}
#' \item{COMMIT_MSG}{Commit message}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' get_git_commit()
#' }
get_git_commit <- function(dir=getwd())
{
  dir <- paste0(dir, "/.git/")
  if(!file.exists(dir)) stop("No git repository found")
  files_to_read <- c("HEAD", "SHA")
  files_content <- list()
  for (f in files_to_read)
  {
    if (f == "SHA") fpath <- paste0(dir, strsplit(files_content$HEAD, " ")[[1]][2])
    if (f == "HEAD") fpath <- paste0(dir, f)
    con <- file(fpath, open="r")
    files_content[[f]] <- readLines(con)
    close(con)
  }

  # get commit msg
  repo_actions <- read.delim(paste0(dir, "logs/HEAD"), header = F)
  resulting_commits <- substr(repo_actions[,1], 42, 81)
  commit_ix <- which(resulting_commits == files_content$SHA)[1]
  files_content$COMMIT_MSG <- as.character(repo_actions[commit_ix,2])

  # out
  files_content
}





#' Create a logger
#'
#' This function creates a logger object that can be used with [logtext()] to
#' write console output to a log file on disk.
#'
#' @param dir path to directory where to log output (default is current directory)
#' @param fname log file name (default is current system date and time)
#' @param add_git_info (logical) whether to add current git commit info
#' to logger and file name (default \code{TRUE})
#' @param git_dir path to git repository (if different from \code{dir})
#' @param init_log_file (logical) whether to log an initial message
#' (by printing the logger to the text file; default is \code{TRUE})
#'
#' @return logger object (list) with elements:
#' \describe{
#' \item{fname}{log file name}
#' \item{dir}{log directory}
#' \item{gitcommit}{(optional) list with elements \code{HEAD}, \code{SHA}, and \code{COMMIT_MSG};
#' see [get_git_commit()]}
#'
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' logger <- get_logger()
#' logger |> logtext(expr=print("hello world!"))
#' }
get_logger <- function(dir=getwd(),
                       fname=NULL,
                       add_git_info=TRUE,
                       git_dir=NULL,
                       init_log_file=TRUE)
{
  # set up logger
  logger <- list()
  if(!file.exists(dir)) stop("dir doesn't exist")
  if(is.null(fname)) fname <- gsub(".*?([0-9]+).*?", "\\1", Sys.time())
  logger <- logger |> c(fname=fname, dir=dir)

  # add git?
  if(add_git_info)
  {
    if(is.null(git_dir) || git_dir == logger$dir) git_dir <- logger$dir else logger$git_dir <- git_dir
    logger$gitcommit <- get_git_commit(git_dir)
    logger$fname <- logger$gitcommit$SHA |> paste(logger$fname, sep="_")
  }

  # init log?
  if(init_log_file) logger |> logtext(expr=print(logger))

  # class
  class(logger) <- "gentleman.logger" |> c(class(logger))

  # out
  logger
}





