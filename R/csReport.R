#' Report results of conditioning data
#'
#' @description Report results of data analyses run with the \code{csCompare}.
#' @param csCompareObj a list or data frame returned from
#' the \code{csCompare} function
#' @param csSensitivityObj Sensitivity analysis results returned from the
#' \code{csSensitivity} function
#' @param dir The directory that the results will be saved. If set to NULL
#' (default), the results are saved in the current working directory.
#' @param fileName The file name of the produced report.

csReport <- function(csCompareObj = NULL, csSensitivity = NULL, dir = NULL,
                     fileName = "report"){
    # Change directory if asked
    if (!is.null(dir)){
      setwd(dir)
    }

    if (base::is.null(csCompareObj) && base::is.null(csSensitivity)){
      rep <- "[No report was produced.]"
    }

    utils::write.table(rep, base::paste0(fileName, ".txt"), quote = FALSE,
                       row.names = FALSE, col.names = FALSE)

}


