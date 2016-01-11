#' Sensitivity analysis for the Bayes Factors of \code{csCompare} results
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

csSensitivity <- function(cs1, cs2, group = NULL, data = NULL,
                                       alternative = "two.sided", conf.level = 0.95,
                                       mu = 0, rscaleSens = c("medium", "wide", "ultrawide")){
  # Since no more groups may be defined, the function terminates if that is
  # the case. Also, if 1 group is selected, then it runs a paired samples
  # t-test.
  if(!is.null(group)){
    ng = base::length(base::unique(stats::na.omit(group)))
    if (ng == 1) {
      group = NULL
    } else {
      if (ng != 2){
        base::stop("You can define up to two groups.
                   Number of groups defined: ", as.character(ng))
      }
      }
    }
  # Based on the group option, it is determined whether a paired samples or
  # between-sample t-test will be performed.
  paired = base::ifelse(base::is.null(group), TRUE, FALSE)

  # You need to define the nullInterval for the BF test based on the 'alternative'
  # option.
  if (alternative == "two.sided"){
    nullInterval = c(-Inf, Inf)
  } else {
    if (alternative == "greater") {
      nullInterval = c(0, Inf)
    } else {
      if (alternative == "less"){
        nullInterval = c(-Inf, 0)
      }
    }
  }

  #' Perform the t-tests based on the options above
  if(paired){
    n1 = base::nrow(stats::na.omit(base::cbind(cs1, cs2)))
    n2 = 0
    ftt = stats::t.test(x = cs1, y = cs2, data = data,
                        alternative = alternative, mu = mu, paired = paired,
                        var.equal = FALSE, conf.level = conf.level)
  } else {
    #cs3 = base::diff(data[, cs1], data[, cs2])
    cs3 = cs1 - cs2
    #cs3 = base::diff(data[, cs1], data[, cs2])
    #groupLevels = base::attr(base::table(data[, group]), "dimnames")[[1]]
    #n1 = length(data[, group][data[, group] == groupLevels[1]])
    #n2 = length(data[, group][data[, group] == groupLevels[2]])
    groupLevels = base::attr(base::table(group), "dimnames")[[1]]
    n1 = length(group[group == groupLevels[1]])
    n2 = length(group[group == groupLevels[2]])
    ftt = stats::t.test(cs3~group, data = data,
                        alternative = alternative, mu = mu, paired = paired,
                        var.equal = FALSE, conf.level = conf.level)
  }

  # Need to compute Bayes factor
  btt = base::sapply(rscaleSens,
   function(x) BayesFactor::ttest.tstat(t = ftt$statistic, n1 = n1, n2 = n2,
                                 nullInterval = nullInterval, rscale = x,
                                 complement = FALSE, simple = FALSE))
  base::colnames(btt) = rscaleSens
  #return(btt)
  # Structure results to a data frame so as to be easier to read
  res = matrix(-999, nrow = length(rscaleSens), ncol = 8)
  colnames(res) = c("nG1", "nG2", "LNI", "HNI", "rscale", "bf10", "bf01",
                    "propError")
  for (i in 1:length(rscaleSens)){
    res[i, ] = c(nG1 = n1, nG2 = n2, LNI = nullInterval[[1]],
    HNI = nullInterval[[2]], rscale = rscaleSens[i],
    bf10 = base::exp(btt[, i][["bf"]]),
    bf01 = 1/base::exp(btt[, i][["bf"]]), propError = btt[, i]$properror)
  }
  res = data.frame(res)
  return(res)
}
