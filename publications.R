message("Gentleman: Publications")

# make table to copy-paste in excel
make_pub_table_from_lavaan_models <- function(ana)
{
  "
  input: ana list (list of lavaan models)
  out: df with IVs and estimate+pvalue for each target
  "
  
  pe <- lapply(ana, parameterEstimates)
  pe <- lapply(pe, function(x) x[x$op=="~",])
  
  # check  that all summaries have the same format
  same_row_orders <- lapply(pe, function(x) all(x[,"rhs"] == pe[[1]][,"rhs"]))
  if (!all(unlist(same_row_orders))) stop("Not all summaries have the same format")
  
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