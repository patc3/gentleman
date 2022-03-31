message("Gentleman: Publications")

# make table to copy-paste in excel
make_pub_table_from_lavaan_models <- function(ana, check_same_format=TRUE)
{
  "
  input: ana list (list of lavaan models)
  out: df with IVs and estimate+pvalue for each target
  "
  
  pe <- lapply(ana, parameterEstimates)
  pe <- lapply(pe, function(x) x[x$op=="~",])
  
  # check  that all summaries have the same format
  same_row_orders <- lapply(pe, function(x) all(x[,"rhs"] == pe[[1]][,"rhs"]))
  if (check_same_format & !all(unlist(same_row_orders))) stop("Not all summaries have the same format")
  
  # get estimates, p-value from each model
  out <- subset(pe[[1]], select=rhs)
  for(s in pe) 
  {
    # postproc p-value
    s$sig <- weights::starmaker(s$pvalue, symbols=c("***", "**", "*", "+"))
    
    # concat est (2 decimals) and sig
    s$est_sig <- paste0(sprintf('%.2f',s$est), s$sig)
    
    # store in table
    v_out <- c("est", "pvalue", "sig", "est_sig")[4]
    out[,ncol(out)+1:length(v_out)] <- s[,v_out]
    
    
  }
  
  # colnames
  if(length(v_out)==1) names(out)[2:ncol(out)] <- names(ana)

  #out
  return(out)
}


# get list of significant/trend effects from a pub table
get_sig_effects_from_pub_table <- function(table, pattern="\\*|\\+", fixed=F)
{
  # get significant predictors with outcome
  sig <- table |> 
    column_to_rownames("Predictor") |> 
    apply(1:2, grepl, pattern=pattern, fixed=fixed) |> 
    which(arr.ind=T) |> 
    (\(m)(data.frame(Predictor=rownames(m), m, row.names=NULL)))() |> 
    select(-row) |> 
    mutate(col=names(table |> select(-1))[col]) |> 
    rename(Outcome=col) |> 
    select(Outcome, Predictor)
  
  # add sign
  sig$Sign <- NA
  for(i in 1:nrow(sig)) sig$Sign[i] <- table |> 
    filter(Predictor == sig$Predictor[i]) |> 
    select(all_of(sig$Outcome[i])) |> 
    substr(1,1)
  sig$Sign <- (sig$Sign == "-") |> ifelse("-", "+")
  
  # out
  return(sig)
}


# compare sig effects from two pub tables
compare_sig_effects_in_two_pub_tables <- function(table1, table2, pattern="*", fixed=T)
{
  # get significant effects in both tables
  sig <- list(table1, table2) |> 
    lapply(\(t) t |> 
             get_sig_effects_from_pub_table(pattern=pattern, fixed=fixed) |>
             with(paste0(Outcome, ": ", Predictor, " (", Sign, ")")))
  
  # output
  cat("_______________________________\n")
  cat(pattern |> paste("in table1 but not in table2:\n"))
  (!sig[[1]] %in% sig[[2]]) |> (\(v)sig[[1]][v])() |> print()
  
  cat("_______________________________\n")
  cat(pattern |> paste("in table2 but not in table1:\n"))
  (!sig[[2]] %in% sig[[1]]) |> (\(v)sig[[2]][v])() |> print()
}
