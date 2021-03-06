#' Plot robustness results
#'
#' @description Plots the results of robustness test
#' @inheritParams csCompare
#' @param rscaleSens the scale factor for the prior used in the Bayesian t.test
#' @param BF01 Should the BF01 be plotted (default is set to TRUE). If FALSE,
#' the BF10 is plotted.
#' @param ylimz the limits of the y-axis (default to NULL).
#' @param sensitivity Should the sensitivity results be returned (default is set
#' to FALSE).
#' @details
#' This plot template is influenced by the JASP way
#' (\url{https://jasp-stats.org/}) for plotting sensitivity analysis results. On the
#' x-axis or the width of the Cauchy's Scale is plotted. On the y-axis either
#' BF01 is plotted (if \code{BF01} is set to TRUE) or
#' BF10 (if \code{BF01} is set to FALSE).
#' @references
#' JASP Team (2019). JASP (Version 0.11.1)[Computer software].
#' 
#' Krypotos, A. M., Klugkist, I., & Engelhard, I. M. (2017). 
#' Bayesian hypothesis testing for human threat conditioning research: 
#' An introduction and the condir R package. 
#' European Journal of Psychotraumatology, 8.
#'
#' @seealso
#' \code{\link[condir]{csCompare}}, \code{\link[condir]{csSensitivity}}
#' @examples
#' set.seed(1000)
#' csRobustnessPlot(cs1 = rnorm(n = 100, mean = 10),
#' cs2 = rnorm(n = 100, mean = 9))
#' @export
csRobustnessPlot <- function(cs1,
                             cs2,
                             group = NULL,
                             data = NULL,
                             alternative = "two.sided",
                             conf.level = 0.95,
                             mu = 0,
                             rscaleSens = c("medium", "wide", "ultrawide"),
                             BF01 = TRUE,
                             ylimz = NULL,
                             sensitivity = FALSE) {
  # You need to define the variables according to whether the 'data'
  # argument is defined or not.
  if (!is.null(data)) {
    cs1 <- data[, deparse(substitute(cs1))]
    cs2 <- data[, deparse(substitute(cs2))]
    
    if (deparse(substitute(group)) != "NULL") {
      group <- as.factor(data[, deparse(substitute(group))])
    }
  }
  
  sensRes <-
    condir::csSensitivity(
      cs1 = cs1,
      cs2 = cs2,
      group = group,
      data = NULL,
      alternative = alternative,
      conf.level = conf.level,
      mu = mu,
      rscaleSens = rscaleSens
    )[[1]]
  
  # Redifine rscale factors if they have been given with their string names.
  # This is in case the csSensitivity returns a factor.
  if (any(levels(sensRes$rscale) %in% c("medium"))) {
    levels(sensRes$rscale) <- c(levels(sensRes$rscale),
                                sqrt(2) / 2)
    sensRes$rscale[which(sensRes$rscale %in% "medium")] <-
      sqrt(2) / 2
  }
  
  if (any(levels(sensRes$rscale) %in% c("wide"))) {
    levels(sensRes$rscale) <- c(levels(sensRes$rscale), 1)
    sensRes$rscale[which(sensRes$rscale %in% "wide")] <- 1
  }
  
  if (any(levels(sensRes$rscale) %in% c("ultrawide"))) {
    levels(sensRes$rscale) <- c(levels(sensRes$rscale),
                                sqrt(2))
    sensRes$rscale[which(sensRes$rscale %in% "ultrawide")] <-
      sqrt(2)
  }
  
  # This is when you have just a string from the csSensitivity
  if (any(sensRes$rscale %in% c("medium"))) {
    sensRes$rscale[which(sensRes$rscale %in% "medium")] <-
      sqrt(2) / 2
  }
  
  if (any(sensRes$rscale %in% c("wide"))) {
    sensRes$rscale[which(sensRes$rscale %in% "wide")] <- 1
  }
  
  if (any(sensRes$rscale %in% c("ultrawide"))) {
    sensRes$rscale[which(sensRes$rscale %in% "ultrawide")] <- sqrt(2)
  }
  
  if (BF01) {
    bf <- "bf01"
    subscript <- "01"
  } else {
    bf <- "bf10"
    subscript <- "10"
  }
  
  bfNum <- as.numeric(as.character(sensRes[[bf]]))
  
  # Set graphic parameters. We need to reset them
  # at exit. We redefine only the relevant parameters,
  # and not all of them, to avoid errors and warnings.
  opmar <- graphics::par()$mar
  opmgp <- graphics::par()$mgp
  on.exit(graphics::par(mar = opmar, mgp = opmgp))
  graphics::par(mar = c(6, 10, 4.1, 8.1), mgp = c(2, 1, .5))
  
  # Limits of y axis is adjusted in case of BF values above 10
  if (!is.null(ylimz)) {
    atYaxis <- ylimz
  } else {
    if (max(bfNum) > 10 || min(bfNum) < 0) {
      ylimz = c(min(bfNum), max(bfNum))
      if (min(bfNum) < 0)
        ylimz[1] <- round(min(bfNum), 0)
      if (max(bfNum) > 10)
        ylimz[2] <- round(max(bfNum), 0)
      atYAxis <- labYAxis <- round(ylimz, 0)
    } else {
      ylimz <- c(0, 10)
      atYAxis <- labYAxis <- c(0, 1, 3, 10)
    }
  }
  
  # Set y axis to scientific or not
  scient <-
    ifelse(max(bfNum) > 100 || min(bfNum) < -100, TRUE, FALSE)
  
  # Main plot
  graphics::plot(
    x = as.numeric(sensRes$rscale),
    y = bfNum,
    type = "b",
    xlab = "",
    ylab = "",
    ylim = ylimz,
    axes = FALSE,
    lwd = 2,
    las = 1,
    bty = "n",
    xpd = TRUE,
    cex.main = 1.5
  )
  graphics::axis(
    side = 1,
    at = sensRes$rscale,
    labels = round(as.numeric(as.character(sensRes$rscale)), 2),
    lwd = 2,
    cex.axis = 1.5
  )
  graphics::axis(
    side = 2,
    at = atYAxis,
    labels = format(labYAxis, scientific = scient),
    lwd = 2,
    cex.axis = 1.5,
    las = 1
  )
  graphics::mtext(
    text = "Cauchy's Scale",
    side = 1,
    line = 3,
    cex = 2,
    xpd = TRUE
  )
  graphics::mtext(
    text = substitute("BF"[subscript,
                           list(subscript = subscript)]),
    side = 2,
    line = 3,
    cex = 2,
    xpd = TRUE
  )
  
  # Print sensitivity results
  if (sensitivity) {
    return(sensRes)
  }
}
