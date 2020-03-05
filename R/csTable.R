#' Produce tables of \code{csCompare} results
#'
#' @description It provides wrapper functions for generating tables of different
#' types of computer languages.
#' @param csCompareObj a list or data frame returned from
#' the \code{csCompare} function
#' @param typeOfTable The type of table to be generated. See details
#'
#' @details \code{csCompare} generates tables of different languages.
#'  The options are \code{latex}, and \code{markdown}.
#' @examples
#' tmp <- csCompare(cs1 = c(1, 2, 3, 1, 4), cs2 = c(10, 12, 12, 31, 13))
#' csTable(tmp)
#' @export
csTable <- function(csCompareObj, typeOfTable = "latex") {
  if (length(typeOfTable) > 1){
      warning("You can produce one type of table at a time. The first
                    type of table of the typeOfTable argument was selected.
                    This is ", typeOfTable[1])
  }
  # Print type of selected table
  if (typeOfTable == "latex"){
    for (i in seq_along(csCompareObj)){
     print(xtable::xtable(as.data.frame(csCompareObj[[i]], 
                                        stringsAsFactors = FALSE)))
     cat("\n\n")
    }
  } else{
    if (typeOfTable == "markdown"){
      for (i in seq_along(csCompareObj)){
        print(knitr::kable(as.data.frame(csCompareObj[[i]], 
                                         stringsAsFactors = FALSE)))
        cat("\n\n")
      }
  }
  }
}
