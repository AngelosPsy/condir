#' Statistically compare two CSs
#'
#' @description Compare two CSs within a frequentist and a Bayesian framework.
#' @param cs1,cs2 a numeric vector of values. If data is defined, it can refer
#' to either the column index or the column name of the data object.See details.
#' @param group column index or name that contain the group data. See details.
#' @param data numeric matrix or data frame that contains all data.
#' @param alternative a character string for the speficication of
#'  the alternative hypothesis. Possible values: \code{"two.sided"} (default),
#'  \code{"greater"} or \code{"less"}.
#' @param conf.level confidence level of the interval.
#' @param mu a numeric value for the mean value or mean difference
#' @param rscale the scale factor for the prior used in the Bayesian t.test.
#' @param descriptives Returns basic descriptive statistics for the dependent
#' variable(s).
#' @details
#' \code{csCompare} performs both a student t-test (using the
#' \code{stats::t.test} function) and a Bayesian t-test (using the
#' \code{BayesFactor::ttest.tstat}). If \code{cs1} and/or \code{cs2}
#' are or refer to multiple columns of a matrix or a data.frame, then
#' the row means are computed before the t-tests are performed.
#' In case \code{group} is \code{NULL},
#' paired-samples t-tests are run. In case the \code{group} is different
#' than \code{NULL}, then the csCompare first computes difference scores between
#'  the cs1 and the cs2 (i.e., cs1 - cs2).
#' In case the group argument is defined
#' but, after removal of NA's (\code{stats::na.omit}), only one group
#' is defined, a paired samples t-test is run.
#' In case of independent samples t-test, the function runs
#' a Welch's t-test.
#' @seealso
#' \code{\link[stats]{t.test}}, \code{\link[BayesFactor]{ttest.tstat}}
#' @examples
#' csCompare(cs1 = c(1, 2, 3, 1, 4), cs2 = c(10, 12, 12, 31, 13))
#' @export
csCompare <- function(cs1, cs2, group = NULL, data = NULL,
                      alternative = "two.sided", conf.level = 0.95,
                      mu = 0, rscale = .707, descriptives = TRUE){
    # Since no more that 2 groups may be defined, the function terminates if
    # that is the case. Also, if 1 group is selected, then it runs a paired
    #  samples t-test.
    if(!base::is.null(group)){
     ng <- base::length(base::unique(stats::na.omit(group)))
     if (ng == 1) {
       group = NULL
     } else {
      if (ng != 2){
        base::stop("You can define up to two groups.
                   Number of groups defined: ", as.character(ng))
    }
    }
    }
    # Compute row means in case cs1 or cs2 refers to more than 1 column.
    if (base::dim(base::as.data.frame(cs1))[2] > 1){
        cs1 <- base::rowMeans(cs1)
    }

    if (base::dim(base::as.data.frame(cs2))[2] > 1){
      cs2 <- base::rowMeans(cs2)
    }

    # Based on the group option, it is determined whether a paired samples or
    # between-sample t-test will be performed.
    paired <- base::ifelse(base::is.null(group), TRUE, FALSE)

    # You need to define the variables according to whether the 'data'
    # argument is defined or not.
    if(!base::is.null(data)){
      cs1 <- data[, cs1]
      cs2 <- data[, cs2]
    }

    # You need to define the nullInterval for the BF test based on the
    # 'alternative' option.
    if (alternative == "two.sided"){
        nullInterval <- c(-Inf, Inf)
    } else {
      if (alternative == "greater"){
          nullInterval <- c(0, Inf)
    } else {
      if (alternative == "less"){
          nullInterval <- c(-Inf, 0)
    }
    }
    }

    # Check normality assumption with shapiro test
    if (paired){
      sT <- stats::shapiro.test(cs1 - cs2)
      sTW1 <- base::as.numeric(sT$statistic)
      sTp1 <- sT$p.value
      sTW2 <- 0
      sTp2 <- 0
    } else {
      cs3 <- cs1 - cs2
      sT <- base::by(cs3, group, stats::shapiro.test, simplify = TRUE)
      sTW1 <- base::as.numeric(sT[[1]]$statistic)
      sTp1 <- sT[[1]]$p.value
      sTW2 <- base::as.numeric(sT[[2]]$statistic)
      sTp2 <- sT[[2]]$p.value
    }

    # Run descriptives
    if (descriptives){
      if(paired){
       desc <- psych::describe(data.frame(cs1, cs2), skew = FALSE,
                              ranges = FALSE)
      } else {
      desc <- base::by(data.frame(cs1, cs2, cs3), group, psych::describe,
                      skew = FALSE, ranges = FALSE)
      }
    }

    # Perform t-test
    if (paired){
      n1 <- base::nrow(stats::na.omit(base::cbind(cs1, cs2)))
      n2 <- 0
      ftt <- stats::t.test(x = cs1, y = cs2, data = data,
                          alternative = alternative, mu = mu, paired = paired,
                          var.equal = FALSE, conf.level = conf.level)
    } else {
     groupLevels <- base::attr(base::table(group), "dimnames")[[1]]
     n1 <- length(group[group == groupLevels[1]])
     n2 <- length(group[group == groupLevels[2]])
     ftt <- stats::t.test(cs3~group, data = data,
                         alternative = alternative, mu = mu, paired = paired,
                         var.equal = FALSE, conf.level = conf.level)
    }

    # Compute Bayes factor
    btt <- BayesFactor::ttest.tstat(t = ftt$statistic, n1 = n1, n2 = n2,
    nullInterval <- nullInterval, rscale = rscale,
    complement <- FALSE, simple = FALSE)

    # Structure results. Then, depending on whether descriptives have been
    # asked or not, more results are generated.
    freq.res <- data.frame(method = ftt$method, alternative = ftt$alternative,
                          WG1 = sTW1, WpG1 = sTp1,
                          WG2 = sTW2, WpG2 = sTp2,
                          null.value = ftt$null.value, LCI = ftt$conf.int[[1]],
                          HCI = ftt$conf.int[[2]],
                          t.statistic = ftt$statistic,
                          df = as.numeric(ftt$parameter),
                          p.value = as.numeric(ftt$p.value), row.names = NULL)
    bayes.res <- data.frame(LNI = nullInterval[[1]],
                            HNI = nullInterval[[2]], rscale = rscale,
                            bf10 = base::exp(btt[["bf"]]),
                            bf01 = 1/base::exp(btt[["bf"]]),
                            propError = btt$properror, row.names = NULL)

    if(descriptives){
      res <- base::list(freq.results = freq.res,
                        bayes.results = bayes.res, descriptives = desc)
    } else {
      res <- base::list(results = res)
    }

    return(res)
}
