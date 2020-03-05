#' Statistically compare CRs towards two CSs
#'
#' @description Compare CRs towards two CSs within a frequentist and
#' a Bayesian framework.
#' @param cs1 a numeric vector of values. If the \code{data} argument is
#' defined, it can refer to either the column index or the column name of
#' the data object. See \code{Details} for more information.
#' @param cs2 a numeric vector of values. If the \code{data} argument is
#' defined, it can refer to either the column index or the column name of
#' the data object. See \code{Details} for more information.
#' @param group column index or name that contain the group data. See
#' \code{Details} for more information.
#' @param data numeric matrix or data frame that contains the relevant data.
#' @param alternative a character string for the speficication of
#'  the alternative hypothesis. Possible values: \code{"two.sided"} (default),
#'  \code{"greater"} or \code{"less"}.
#' @param conf.level Interval's confidence level.
#' @param mu a numeric value for the mean value or mean difference.
#' @param rscale the scale factor for the prior used in the Bayesian t.test.
#' @param descriptives Returns basic descriptive statistics for \code{cs1} and
#' \code{cs2}.
#' @param out.thres The threeshold for detecting outliers (default is 3). If set
#' to 0, no outliers analysis will be performed. See \code{Details} below for
#' more information.
#' @param boxplot Should a boxplot of the variables be produced
#' (default is TRUE)?
#' @return
#' The function returns (at least) 3 list objects. These are: \code{descriptives},
#' \code{freq.results}, and \code{bayes.results}. In case outliers are detected,
#' then the outlier analyses are returned as well with the name \code{res.out}
#' as prefix to all list objects. For example, the descriptive statistics of
#' the outlier analyses, can be indexed by using
#' \code{obj$res.out$descriptives}, with obj being the object of the csCompare
#' results.
#'
#' The values of the \code{descriptives} are described in
#' \code{psych::describe}.
#'
#' The values of the \code{freq.results} are:
#' \code{method}: which test was run.
#'
#' \code{alternative}: the alternative hypothesis.
#'
#' \code{WG1, WG2}: the Shapiro test values, separately for group 1 and group 2.
#' In case of a paired-samples t-test, the WG2 is 0.
#'
#' \code{WpG1, WpG2}: the p-values of Shapiro test, separately for group 1
#' and group 2. In case of a paired-samples t-test, the WpG2 is 0.
#'
#' \code{null.value}: The value defined by \code{mu} (see above).
#'
#' \code{LCI, HCI}: The low (\code{LCI}) and high (\code{HCI}) bounds
#' of the confidence intervals.
#'
#' \code{t.statistic}: Logical.
#'
#' \code{df}: The degrees of freedom of the t-test performed.
#'
#' \code{p.value}: The p-value of the performed t-test.
#'
#' \code{cohenD}: The Cohen's d for the performed t-test.
#'
#' \code{cohenDM}: The magnitude of the resulting Cohen's d.
#'
#' \code{hedgesG}: The Hedge's g for the performed t-test.
#'
#' \code{hedgesGM}: The magnitude of the resulting Hedge's g.
#'
#' The values of the \code{bayes.results} are:
#'
#' \code{LNI, HNI}: The low (\code{LNI}) and high (\code{HNI}) intervals of the
#' hypothesis to test.
#'
#' \code{rscale}: The used scale (see \code{rscale} argument above).
#'
#' \code{bf10}: The BF10.
#'
#' \code{bf01}: The BF01.
#'
#' \code{propError}: The proportional error of the computed Bayes factor.
#'
#' @details
#' \code{csCompare} performs both a student t-test (using the
#' \code{stats::t.test} function) and a Bayesian t-test (using the
#' \code{BayesFactor::ttest.tstat}). If \code{cs1} and/or \code{cs2}
#' are or refer to multiple columns of a matrix or a data.frame, then
#' the row means are computed before the t-tests are performed.
#' In case \code{group} is \code{NULL},
#' paired-samples t-tests will be run. In case the \code{group} is different
#' than \code{NULL}, then the csCompare first computes difference scores between
#'  the cs1 and the cs2 (i.e., cs1 - cs2).
#' In case the group argument is defined
#' but, after removal of NA's (\code{stats::na.omit}), only one group
#' is present, a paired samples t-test is run.
#' In case of independent samples t-test, the function runs
#' a Welch's t-test.
#'
#' Regarding outliers, those are detected based on the deviations from the
#' standardized residuals of each test. For example, in case of a paired-samples
#' t-test, the \code{csCompare} function will run an additional regression for
#' detecting deviations (defined in the \code{out.thres} argument)
#' from the standardized residuals. The detected outliers are removed from both
#' the frequentists and Bayesian analyses.
#'
#' @references
#' Krypotos, A. M., Klugkist, I., & Engelhard, I. M. (2017). 
#' Bayesian hypothesis testing for human threat conditioning research: 
#' An introduction and the condir R package. 
#' European Journal of Psychotraumatology, 8.
#'
#' Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009).
#' Bayesian t-tests for accepting and rejecting the null hypothesis.
#' Psychonomic Bulletin & Review, 16, 225-237
#'
#' @seealso
#' \code{\link[stats]{t.test}}, \code{\link[BayesFactor]{ttest.tstat}}
#' @examples
#' set.seed(1000)
#' csCompare(cs1 = rnorm(n = 100, mean = 10), cs2 = rnorm(n = 100, mean = 9))
#' @export
csCompare <- function(cs1,
                      cs2,
                      group = NULL,
                      data = NULL,
                      alternative = "two.sided",
                      conf.level = 0.95,
                      mu = 0,
                      rscale = .707,
                      descriptives = TRUE,
                      out.thres = 3,
                      boxplot = TRUE) {
  # You need to define the variables according to whether the 'data'
  # argument is defined or not.
  if (!is.null(data)) {
    cs1 <- data[, deparse(substitute(cs1))]
    cs2 <- data[, deparse(substitute(cs2))]
    
    if (deparse(substitute(group)) != "NULL") {
      group <- as.factor(data[, deparse(substitute(group))])
    }
  }
  
  # Since no more that 2 groups may be defined, the function terminates if
  # that is the case. Also, if 1 (or 0) group is selected, then it runs a paired
  #  samples t-test.
  if (!is.null(group)) {
    ng <- length(unique(stats::na.omit(group)))
    if (ng %in% c(0, 1) || group[1] == "NULL") {
      group = NULL
    } else {
      if (ng > 2) {
        stop("You can define up to two groups.
             Number of groups defined: ",
             as.character(ng))
      }
    }
    }
  
  # Compute row means in case cs1 or cs2 refers to more than 1 column.
  if (dim(as.data.frame(cs1, stringsAsFactors = FALSE))[2] > 1) {
    cs1 <- rowMeans(cs1)
  }
  
  if (dim(as.data.frame(cs2, stringsAsFactors = FALSE))[2] > 1) {
    cs2 <- rowMeans(cs2)
  }
  
  # Based on the group option, it is determined whether a paired samples or
  # between-sample t-test will be performed.
  paired <- ifelse(is.null(group), TRUE, FALSE)
  
  # You need to define the nullInterval for the BF test based on the
  # 'alternative' option.
  if (alternative == "two.sided") {
    nullInterval <- c(-Inf, Inf)
  } else {
    if (alternative == "greater") {
      nullInterval <- c(0, Inf)
    } else {
      if (alternative == "less") {
        nullInterval <- c(-Inf, 0)
      }
    }
  }
  
  # Check normality assumption with shapiro test
  if (paired) {
    tmp <- cs1 - cs2
    sT <- stats::shapiro.test(tmp)
    sTW1 <- as.numeric(sT$statistic)
    sTp1 <- sT$p.value
    sTW2 <- 0
    sTp2 <- 0
  } else {
    cs3 <- cs1 - cs2
    sT <- by(cs3, group, stats::shapiro.test, simplify = TRUE)
    sTW1 <- as.numeric(sT[[1]]$statistic)
    sTp1 <- sT[[1]]$p.value
    sTW2 <- as.numeric(sT[[2]]$statistic)
    sTp2 <- sT[[2]]$p.value
  }
  
  # Run descriptives
  if (descriptives) {
    if (paired) {
      desc <- psych::describe(
        data.frame(cs1, cs2,
                   stringsAsFactors = FALSE),
        skew = FALSE,
        ranges = TRUE
      )
    } else {
      descInit <- by(
        data = data.frame(cs1, cs2, cs3,
                          stringsAsFactors = FALSE),
        INDICES = group,
        FUN =  psych::describe,
        skew = FALSE,
        ranges = TRUE
      )
      desc <- list(descInit[[1]], descInit[[2]])
      names(desc) <- names(descInit)
    }
  }
    
    # Perform t-test, including the computation of Cohen's D
    if (paired) {
      n1 <- nrow(stats::na.omit(cbind(cs1, cs2)))
      n2 <- 0
      ftt <- stats::t.test(
        x = cs1,
        y = cs2,
        data = data,
        alternative = alternative,
        mu = mu,
        paired = paired,
        var.equal = FALSE,
        conf.level = conf.level
      )
      effsize::cohen.d(cs1, cs2, paired = TRUE)
      cD <- effsize::cohen.d(d = cs1, f = cs2, paired = TRUE)
      hG <- effsize::cohen.d(
        d = cs1,
        f = cs2,
        paired = TRUE,
        hedges.correction = TRUE
      )
    } else {
      groupLevels <- attr(table(group), "dimnames")[[1]]
      n1 <- length(group[group == groupLevels[1]])
      n2 <- length(group[group == groupLevels[2]])
      ftt <- stats::t.test(
        cs3 ~ group,
        data = data,
        alternative = alternative,
        mu = mu,
        paired = paired,
        var.equal = FALSE,
        conf.level = conf.level
      )
      cD <- effsize::cohen.d(d = cs1, f = cs2, paired = FALSE)
      hG <- effsize::cohen.d(
        d = cs1,
        f = cs2,
        paired = FALSE,
        hedges.correction = TRUE
      )
      
    }
    
    # Compute Bayes factor
    btt <- BayesFactor::ttest.tstat(
      t = ftt$statistic,
      n1 = n1,
      n2 = n2,
      nullInterval = nullInterval,
      rscale = rscale,
      complement = FALSE,
      simple = FALSE
    )
    

    # Structure results. Then, depending on whether descriptives have been
    # asked or not, more results are generated.
    freq.res <- data.frame(method = ftt$method, alternative = ftt$alternative,
                          WG1 = sTW1, WpG1 = sTp1,
                          WG2 = sTW2, WpG2 = sTp2,
                          null.value = ftt$null.value, LCI = ftt$conf.int[[1]],
                          HCI = ftt$conf.int[[2]],
                          t.statistic = ftt$statistic,
                          df = as.numeric(ftt$parameter),
                          p.value = as.numeric(ftt$p.value),
                          cohenD = as.numeric(cD$estimate),
                          cohenDM = as.character(cD$magnitude),
                          hedgesG = as.numeric(hG$estimate),
                          hedgesGM = as.character(hG$magnitude),
                            row.names = NULL)
    bayes.res <- data.frame(LNI = nullInterval[[1]],
                            HNI = nullInterval[[2]], rscale = rscale,
                            bf10 = exp(btt[["bf"]]),
                            bf01 = 1/exp(btt[["bf"]]),
                            propError = btt$properror, row.names = NULL,
                            stringsAsFactors = FALSE)

    if(descriptives){
      res <- list(descriptives = desc, freq.results = freq.res,
                        bayes.results = bayes.res)
    } else {
      res <- list(results = res)
    }

    if (boxplot){
      # Generate boxplots
      opmar <- graphics::par()$mar
      opmgp <- graphics::par()$mgp
      on.exit(graphics::par(mar = opmar, mgp = opmgp))
      graphics::par(mar = c(6, 10, 4.1, 8.1), mgp = c(2, 1, .5))

      if (paired){
        graphics::boxplot(x = data.frame(cs1, cs2, stringsAsFactors = FALSE), cex.main = 1.5, las = 1,
                          cex.lab = 2, cex.axis = 1, bty = "n", lwd = 1,
                          xpd = TRUE, pch = 19)
      } else{
        graphics::boxplot(data.frame(cs1, cs2, cs3, stringsAsFactors = FALSE), cex.main = 1.5, las = 1,
                          cex.lab = 2, cex.axis = 1, bty = "n", lwd = 1,
                          xpd = TRUE, pch = 19)
      }
    }

    # Outlier analysis
    if (out.thres != 0) {
      if (paired) {
        cout <- cs1 - cs2
        outz <- stats::rstandard(stats::lm(cout ~ 1))
        out.HCI <- which(outz > out.thres)
        out.LCI <- which(outz < (-out.thres))
        if (length(out.HCI) == 0 && length(out.LCI) == 0) {
          out.present <- FALSE
        } else {
          out.present <- TRUE
          cs1.out <- cs1[-c(out.HCI, out.LCI)]
          cs2.out <- cs2[-c(out.HCI, out.LCI)]
          compare.out <-
            condir::csCompare(
              cs1 = cs1.out,
              cs2 = cs2.out,
              group = group,
              data = NULL,
              alternative = alternative,
              conf.level = conf.level,
              mu = mu,
              rscale = rscale,
              descriptives = descriptives,
              boxplot = boxplot,
              out.thres = 0
            )
        }
        
      } else {
        cs3 <- cs1 - cs2
        cout <- cs3
        outz <- stats::rstandard(stats::lm(cout ~ group))
        out.HCI <- which(outz > out.thres)
        out.LCI <- which(outz < (-out.thres))
        cs1.out <- cs1[-c(out.HCI, out.LCI)]
        cs2.out <- cs2[-c(out.HCI, out.LCI)]
        group.out <- group[-c(out.HCI, out.LCI)]
        
        if (length(out.HCI) == 0 && length(out.LCI) == 0) {
          out.present <- FALSE
        } else {
          out.present <- TRUE
          compare.out <-
            condir::csCompare(
              cs1 = cs1.out,
              cs2 = cs2.out,
              group = group.out,
              data = NULL,
              alternative = alternative,
              conf.level = conf.level,
              mu = mu,
              rscale = rscale,
              descriptives = descriptives,
              boxplot = boxplot,
              out.thres = 0
            )
        }
      }
    } else {
      # This is the case you do not want outliers to be detected
      out.present = FALSE
    }
    if (out.present) {
      res$res.out = compare.out
    }
    
    # Create csCompare class.
    attr(res, "class") <- "csCompare"
    
    return(res
    )
}
