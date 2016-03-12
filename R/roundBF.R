#' Local function for determining how BF is reported
#'
#' @description Function used for determining the symbol, and the form, of the
#' BF reported. This function is not be exported in the NAMESPACE.
#' @param bf Logical. The BF.
#' @rscale rscale The rscale that was used.
#' @rscale BF01 Whether the BF01 (default) should be reported or not
roundBF = function(bf, rscale, BF01 = TRUE){
  bf.round <- round(as.numeric(as.character(bf)), 3)
  if (bf > 1000){
    r.bf <- paste(" > ", 1000)
  } else if (bf < 0.0001) {
    r.bf <- paste(" < ", 0.0001)
  } else{
    r.bf <- paste(" = ", bf.round)
  }

  BFreport <- ifelse(BF01, "BF01", "BF10")
  # Format final results
  res <- paste0(BFreport, "(", rscale, ") ", r.bf, collapse = "")
  return(res)
}
