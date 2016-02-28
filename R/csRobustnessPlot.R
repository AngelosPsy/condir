#' Plot robusteness results
#'
#' @description Plots the results of robustness test
#' @param cs1,cs2 a numeric vector of values. If data is defined, it can refer
#' to either the column index or the column name of the data object.
#' @param group column index or name that contain the group data. See details.
#' @param data numeric matrix or data frame that contains all data.
#' @param alternative a character string for the speficication of
#'  the alternative hypothesis. Possible values: \code{"two.sided"} (default),
#'  \code{"greater"} or \code{"less"}.
#' @param mu a numeric value for the mean value or mean difference.
#' @param conf.level confidence level of the interval.
#' @param rscaleSens the scale factor for the prior used in the Bayesian t.test
#' @param BF01 Should the BF01 be plotted (Default is set to TRUE). If FALSE,
#' the BF10 is plotted.
#' @param ylimz the limits of the y-axis.
#' @param ... Further plotting arguments.
#' @details
#' This plot is influenced by the JASP way to plot robustness results. On the
#' x-axis or the width of the Cauchy's Scale is plotted. On the y-axis either
#' BF01 is plotted (if \code{BF01} is set to TRUE) or
#' BF10 (if \code{BF01} is set to FALSE).
#' @seealso
#' \code{\link[condir]{csCompare}}, #' \code{\link[condir]{csSensitivity}}
#' @examples
#' csRobustnessPlot(cs1 = rnorm(10, 10, 2), cs2 = rnorm(10, 9, 3))
#' @export
csRobustnessPlot <- function(cs1, cs2, group = NULL, data = NULL,
                          alternative = "two.sided", conf.level = 0.95, mu = 0,
                          rscaleSens = c("medium", "wide", "ultrawide"),
                          BF01 = TRUE, ylimz = c(0, 10), ...){

  sensRes <- condir::csSensitivity(cs1 = cs1, cs2 = cs2, group = group,
                                   data = data, alternative = alternative,
                                   conf.level = conf.level, mu = mu,
                                   rscaleSens = rscaleSens)

    # Redifine rscale factors if they have been given with their string names
    levels(sensRes$rscale)
    if (any(levels(sensRes$rscale) %in% c("medium"))){
      levels(sensRes$rscale) <- c(levels(sensRes$rscale),
                                        sqrt(2)/2)
      sensRes$rscale[which(sensRes$rscale %in% "medium")] <-
        sqrt(2)/2
    }

    if (any(levels(sensRes$rscale) %in% c("wide"))){
      levels(sensRes$rscale) <- c(levels(sensRes$rscale), 1)
      sensRes$rscale[which(sensRes$rscale %in% "wide")] <- 1
    }

    if (any(levels(sensRes$rscale) %in% c("ultrawide"))){
      levels(sensRes$rscale) <- c(levels(sensRes$rscale),
                                        sqrt(2))
      sensRes$rscale[which(sensRes$rscale %in% "ultrawide")] <- sqrt(2)
    }

    if (BF01){
      bf <- "bf01"
      subscript <- "01"
    } else {
      bf <- "bf10"
      subscript <- "10"
    }

    bfNum <- as.numeric(as.character(sensRes[[bf]]))

    # Set graphic parameters
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op))
    graphics::par(mar = c(6, 7, 4.1, 8.1), cex.main = 1.5, las=1, cex.lab = 2,
                  mgp = c(2, 1, .5), cex.axis = 1, bty = "n", lwd = 1, xpd = T,
                  pch = 19)

    # Limits of y axis is adjusted in case of BF values above 10
    if(exists(as.character(ylimz))){
      atYaxis <- ylimz
    } else {
     if(max(bfNum) > 10 || min(bfNum) < 0){
       ylimz = c(min(bfNum), max(bfNum))
       if (min(bfNum) < 0) ylimz[1] <- round(min(bfNum), 0)
       if (max(bfNum) > 10) ylimz[2] <- round(max(bfNum), 0)
       atYAxis <- labYAxis <- round(ylimz, 0)
     } else {
       ylimz <- c(0, 10)
       atYAxis <- labYAxis <- c(0, 1, 3, 10)
     }
     }

    # Main plot
    graphics::plot(x = as.numeric(sensRes$rscale), y = bfNum,
                   type = "b", xlab = "", ylab = "", ylim = ylimz,
                   axes = FALSE, lwd = 2, ...)
    graphics::axis(side = 1, at = sensRes$rscale,
                   labels = round(as.numeric(
                     as.character(sensRes$rscale)), 2),
                   lwd = 2, cex.axis = 1.5)
    graphics::axis(side = 2, at = atYAxis,
                   labels = labYAxis, lwd = 2,
                   cex.axis = 1.5)
    graphics::mtext(text = "Cauchy's Scale", side = 1, line = 3,
                    cex = 2)
    graphics::mtext(text = substitute("BF"[subscript,
                    list(subscript = subscript)]),
                    side = 2, line = 3, cex = 2)
}
