#' Plot robusteness results
#'
#' @description Perform a sensitivity analysis for the Bayes factors computed
#' with the \code{csCompare} results
#' @param cs1,cs2 a numeric vector of values. If data is defined, it can refer
#' to either the column index or the column name of the data object.
#' @param group column index or name that contain the group data. See details.
#' @param data numeric matrix or data frame that contains all data.
#' @param alternative a character string for the speficication of
#'  the alternative hypothesis. Possible values: \code{"two.sided"} (default),
#'  \code{"greater"} or \code{"less"}.
#' @param mu a numeric value for the mean value or mean difference
#' @param rscaleSens the scale factor for the prior used in the Bayesian t.test
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
#' @return The function returns a data frame with the results of the student
#' t-test and the Bayesian t-test.
#' @seealso
#' \code{\link[condir]{csCompare}}, \code{\link[stats]{t.test}},
#' \code{\link[BayesFactor]{ttest.tstat}}

csRobustnessPlot <- function(cs1, cs2, group = NULL, data = NULL,
                          alternative = "two.sided", conf.level = 0.95, mu = 0,
                          rscaleSens = c("medium", "wide", "ultrawide")){

  sensRes <- csSensitivity(cs1 = cs1, cs2 = cs2, group = group, data = data,
                alternative = alternative, conf.level = conf.level, mu = mu,
                rscaleSens)

    #' Redifine rscale factors if they have been given with their string names
    levels(sensRes$rscale)
    if (any(levels(sensRes$rscale) %in% c("medium"))){
      levels(sensRes$rscale) <- c(levels(sensRes$rscale), sqrt(2)/2)
      sensRes$rscale[which(sensRes$rscale %in% "medium")] <- sqrt(2)/2
    }

    if (any(levels(sensRes$rscale) %in% c("wide"))){
      levels(sensRes$rscale) <- c(levels(sensRes$rscale), 1)
      sensRes$rscale[which(sensRes$rscale %in% "wide")] <- 1
    }

    if (any(levels(sensRes$rscale) %in% c("ultrawide"))){
      levels(sensRes$rscale) <- c(levels(sensRes$rscale), sqrt(2))
      sensRes$rscale[which(sensRes$rscale %in% "ultrawide")] <- sqrt(2)
    }

    # Set graphic parameters
    op <- graphics::par(no.readonly = TRUE)
    base::on.exit(graphics::par(op))
    graphics::par(mar=c(5.1, 4.1, 4.1, 8.1), cex.main = 1.5, las=1, cex.lab = 2,
                  mgp = c(2,1,.5), cex.axis = 1, bty = "n", lwd = 1, xpd = T, pch = 19)
  plot(x = as.numeric(sensRes$rscale), y = as.numeric(sensRes$bf10), type = "b")



}
