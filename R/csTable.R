#' Produce tables of \code{csCompare} results
#'
#' @description It provides wrapper functions for generating tables of different
#' types of computer languages.
#' @param csCompareObje a data frame returned from the \code{csCompare} object
#' @param typeOfTable The type of table to be generated. See details
#'
#' @details \code{csCompare} generates tables of different languages.
#'  The options are \code{latex}, and \code{markdown}.
#'

csTable <- function(csCompareObje, typeOfTable = c("latex")) {
  if (length(typeOfTable) > 1){
      base::warning("You can produce one type of table at a time. The first
                    type of table of the typeOfTable argument was selected.
                    This is ", typeOfTable[1])
  }
  # Print type of selected table
  if (typeOfTable == "latex"){
    xtable::xtable(csCompareObje)
  } else{
    if (typeOfTable == "markdown"){
      knitr::kable(csCompareObje)
  }
  }
}
