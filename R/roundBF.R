#' Local function for determining how BF is reported
#'
#' @description Function used for determining the symbol, and the form, of the
#' BF reported. This function is not be exported in the NAMESPACE.
#' @param bf Logical. The BF.
roundBF = function(bf){
  bf.round <- round(as.numeric(as.character(bf)), 3)
  if (bf > 1000){
    r.bf <- paste(" > ", 1000)
  } else if (bf < 0.0001) {
    r.bf <- paste(" < ", 0.0001)
  } else{
    r.bf <- paste(" = ", bf.round)
  }

  return(r.bf)
}
