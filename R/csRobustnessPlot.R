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
#' This plot is influenced by the JASP way to plot robustness results.
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
    base::levels(sensRes$rscale)
    if (base::any(base::levels(sensRes$rscale) %in% c("medium"))){
      base::levels(sensRes$rscale) <- c(levels(sensRes$rscale),
                                        base::sqrt(2)/2)
      sensRes$rscale[base::which(sensRes$rscale %in% "medium")] <- base::sqrt(2)/2
    }

    if (base::any(base::levels(sensRes$rscale) %in% c("wide"))){
      base::levels(sensRes$rscale) <- c(base::levels(sensRes$rscale), 1)
      sensRes$rscale[base::which(sensRes$rscale %in% "wide")] <- 1
    }

    if (any(base::levels(sensRes$rscale) %in% c("ultrawide"))){
      base::levels(sensRes$rscale) <- c(base::levels(sensRes$rscale),
                                        base::sqrt(2))
      sensRes$rscale[which(sensRes$rscale %in% "ultrawide")] <- base::sqrt(2)
    }

    # Set graphic parameters
    op <- graphics::par(no.readonly = TRUE)
    base::on.exit(graphics::par(op))
    graphics::par(mar=c(5.1, 4.1, 4.1, 8.1), cex.main = 1.5, las=1, cex.lab = 2,
                  mgp = c(2,1,.5), cex.axis = 1, bty = "n", lwd = 1, xpd = T,
                  pch = 19)
    graphics::plot(x = as.numeric(sensRes$rscale), y = as.numeric(sensRes$bf10),
                   type = "b", xlab = "", ylab = "", ylim = c(-10, 10),
                   yaxt = "n")
    graphics::axis(side = 2, at = c(-10, -3, 1, 3, 10), labels = c("1/10", "1/3",
                   1, "3", "10"))
    graphics::mtext(text = "Scale factor of Cauchy", side = 1, line = 3, cex = 2)

}
