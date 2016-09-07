#' Plot CRs for each CS
#'
#' @description Plot the mean of median of each CRs, for each CS
#' @inheritParams csCompare
#' @param ylab Label for the x-axis
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
#' @seealso
#' \code{\link[stats]{t.test}}, \code{\link[BayesFactor]{ttest.tstat}}
#' @examples
#' csPlot(cs1 = rnorm(n = 100, mean = 10), cs2 = rnorm(n = 100, mean = 9))
#' @export
csPlot <- function(cs1, cs2, group = NULL, data = NULL, ylab = "CRs",
                   col = c("black", "grey"), legend = c("cs1", "cs2")){
  # Since no more groups may be defined, the function terminates if that is
  # the case. Also, if 1 group is selected, then it runs a paired samples
  # t-test.
  if(!is.null(group)){
    ng <- length(unique(stats::na.omit(group)))
    if (ng %in% c(0, 1)) {
      group <- NULL
    } else {
      if (ng > 2){
        stop("You can define up to two groups.
                   Number of groups defined: ", as.character(ng))
      }
    }
  }

  # You need to define the variables according to whether the 'data'
  # argument is defined or not.
  if(!is.null(data)){
    cs1 <- data[, cs1]
    cs2 <- data[, cs2]
  }

  # Run descriptives
  if(!is.null(group)){
    desc <- by(data.frame(cs1, cs2), group, psych::describe,
                     skew = FALSE, ranges = FALSE)
  } else {
    desc <- psych::describe(data.frame(cs1, cs2), skew = FALSE,
                            ranges = FALSE)
  }

  # Set graphic parameters
  opmar <- graphics::par()$mar
  opmgp <- graphics::par()$mgp
  on.exit(graphics::par(mar = opmar, mgp = opmgp))
  graphics::par(mar = c(6, 10, 4.1, 8.1), mgp = c(2, 1, .5))

  if (length(col) != length(legend)){
    warning("The length of the color argument,
                 (i.e., ", length(col), "), is different than the length
                  of the legend argument, (i.e.,", length(legend), ").
            The plotted legends may not correspond to the right bars.")
  }

  if(!is.null(group)){
    meanz <- c(desc[[1]]["mean"], desc[[2]]["mean"])
    sdz <- c(desc[[1]]["se"], desc[[2]]["se"])
    graphics::barplot(matrix(unlist(meanz), 2, 2), col = col,
                      beside = T,
                      names.arg = unlist(attr(table(group), "dimnames")),
                      cex.names = 1.5, cex.main = 1.5, las=1, cex.lab = 2,
                      cex.axis = 1, bty = "n", lwd = 1, xpd = T,
                      pch = 19)
    graphics::mtext(ylab, 2, line = 3, cex = 1.5, las=0)
    graphics::legend(x = "topright", legend = legend, inset=c(-0.2,0),
                     title = "Stimulus", bty = "n", pch = 15,
                     bg = "black", col = col, cex = 1.5)
  } else {
    meanz <- desc["mean"]
    sdz <- desc["se"]
    graphics::barplot(as.numeric(unlist(meanz)), col = col,
                      names.arg = NULL, cex.main = 1.5, las=1, cex.lab = 2,
                      cex.axis = 1, bty = "n", lwd = 1, xpd = T,
                      pch = 19)
    graphics::mtext(ylab, 2, line = 3, cex = 1.5, las=0)
    graphics::legend(x = "topright", legend = legend, inset=c(-0.2, 0),
                     title = "Stimulus", bty = "n", pch = 15,
                     bg = "black", col = col, cex = 1.5, xpd = TRUE)
  }
}
