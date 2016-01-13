#' Plot CRs for each CS
#'
#' @description Plot the mean of median of each CRs, for each CS
#' @param cs1,cs2 a numeric vector of values. If data is defined, it can refer
#' to either the column index or the column name of the data object.
#' @param group column index or name that contain the group data. See details.
#' @param data numeric matrix or data frame that contains all data.
#' @param col The color of the error bars to be used (either given as a numeric
#'  vector or a character string). The length of the chosen colors should
#'  be equal to the length of the legend names otherwise a warning is returned.
#' @param legend The legend names to be used. The length of the legend labels
#' should be the same as the length of the color string, otherwise
#' a warning is returned.
#' @details
#' \code{csCompare} performs both a student t-test (using the
#' \code{stats::t.test} function) and a Bayesian t-test (using the
#' \code{BayesFactor::ttest.tstat}). In case \code{group} is not defined,
#' paired-samples t-tests are run. In case the \code{group} is
#' defined, then the csCompare first computes difference scores between the cs1
#' and the cs2
#' (i.e., cs1 - cs2).
#' In case the group argument is defined
#' but, after removal of NA's (\code{stats::na.omit}), only one group
#' is defined, a paired samples t-test is run.
#' @param ... Additional plot arguments.
#' @seealso
#' \code{\link[stats]{t.test}}, \code{\link[BayesFactor]{ttest.tstat}}
#' @examples
#' csPlot(cs1 = rnorm(10, 10, 2), cs2 = rnorm(10, 9, 3))
#' @export
csPlot <- function(cs1, cs2, group = NULL, data = NULL,
                   col = c("black", "grey"), legend = c("cs1", "cs2"), ...){
  # Since no more groups may be defined, the function terminates if that is
  # the case. Also, if 1 group is selected, then it runs a paired samples
  # t-test.
  if(!base::is.null(group)){
    ng <- base::length(base::unique(stats::na.omit(group)))
    if (ng == 1) {
      group <- NULL
    } else {
      if (ng != 2){
        base::stop("You can define up to two groups.
                   Number of groups defined: ", as.character(ng))
      }
    }
  }

  # You need to define the variables according to whether the 'data'
  # argument is defined or not.
  if(!base::is.null(data)){
    cs1 <- data[, cs1]
    cs2 <- data[, cs2]
  }

  # Run descriptives
  if(!base::is.null(group)){
    desc <- base::by(data.frame(cs1, cs2), group, psych::describe,
                     skew = FALSE, ranges = FALSE)
  } else {
    desc <- psych::describe(data.frame(cs1, cs2), skew = FALSE,
                            ranges = FALSE)
  }

  # Set graphic parameters
  op <- graphics::par(no.readonly = TRUE)
  base::on.exit(graphics::par(op))
  graphics::par(mar=c(5.1, 4.1, 4.1, 8.1), cex.main = 1.5, las=1, cex.lab = 2,
                mgp = c(2,1,.5), cex.axis = 1, bty = "n", lwd = 1, xpd = T,
                pch = 19)

  if (base::length(col) != base::length(legend)){
    base::warning("The length of the color argument,
                 (i.e., ", base::length(col), "), is different than the length
                  of the legend argument, (i.e.,", base::length(legend), ").
            The plotted legends may not correspond to the right bars.")
  }

  if(!base::is.null(group)){
    meanz <- c(desc[[1]]["mean"], desc[[2]]["mean"])
    sdz <- c(desc[[1]]["se"], desc[[2]]["se"])
    print(meanz)
    print(sdz)
    graphics::barplot(base::matrix(base::unlist(meanz), 2, 2), col = col,
                      beside = T, ...)
    graphics::legend(x = "topright", legend = legend, inset=c(-0.2,0),
                     title = "Stimulus", bty = "n", pch = 15,
                     bg = "black", col = col, cex = 1.5)
  } else {
    meanz <- desc["mean"]
    sdz <- desc["se"]
    graphics::barplot(base::unlist(meanz), col = col, beside = T, ...)
    graphics::legend(x = "topright",  legend = legend, inset=c(-0.2,0),
                     title = "Stimulus", bty = "n", pch = 15,
                     bg = "black", col = col, cex = 1.5)
  }

}
