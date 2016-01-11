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
                          alternative = "two.sided", conf.level = 0.95, mu = 0,
                          rscaleSens = c("medium", "wide", "ultrawide")){

  #' Extract t statistic
  ftt <- csCompare(cs1 = cs1, cs2 = cs2, group = group, data = data,
                  alternative = alternative, conf.level = conf.level, mu = mu,
                  descriptives = TRUE)

  #' Need to define the number of participants for each group
  paired <- base::ifelse(base::is.null(group), TRUE, FALSE)
  if (paired){
    n1 <- base::nrow(stats::na.omit(base::cbind(cs1, cs2)))
    n2 <- 0
   } else {
    groupLevels <- base::attr(base::table(group), "dimnames")[[1]]
    n1 <- length(group[group == groupLevels[1]])
    n2 <- length(group[group == groupLevels[2]])
  }

  # Need to compute Bayes factor
  btt = base::sapply(rscaleSens,
   function(x) BayesFactor::ttest.tstat(t = ftt$freq.results$t.statistic,
                                        n1 = n1, n2 = n2,
                                        nullInterval = c(ftt$bayes.res$LN1,
                                                         ftt$bayes$res$HN1),
                                                         rscale = x,
                                                         complement = FALSE,
                                                         simple = FALSE))
  base::colnames(btt) <- rscaleSens
  # Structure results to a data frame so as to be easier to read
  res <- matrix(-999, nrow = length(rscaleSens), ncol = 8)
  colnames(res) <- c("nG1", "nG2", "LNI", "HNI", "rscale", "bf10", "bf01",
                    "propError")

  for (i in 1:length(rscaleSens)){
    res[i, ] <- c(nG1 = n1, nG2 = n2, LNI = as.character(ftt$bayes.res$LNI),
    HNI = as.character(ftt$bayes.res$HNI), rscale = rscaleSens[i],
    bf10 = base::exp(btt[, i][["bf"]]),
    bf01 = 1/base::exp(btt[, i][["bf"]]), propError = btt[, i]$properror)
  }

  res = data.frame(res)

  return(res)
}
