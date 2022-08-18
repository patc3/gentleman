logtext <- function(expr,
                    logger=get_logger(add_git_info = FALSE, init_log_file = FALSE),
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





get_logger <- function(dir=getwd(),
                       fname=NULL,
                       add_git_info=TRUE,
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
    logger$gitcommit <- get_git_commit(dir)
    logger$fname <- logger$gitcommit$SHA |> paste(logger$fname, sep="_")
  }

  # init log?
  if(init_log_file) logger |> logtext(expr=print(logger))

  # class
  class(logger) <- "gentleman.logger" |> c(class(logger))

  # out
  logger
}





