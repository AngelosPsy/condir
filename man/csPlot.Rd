% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csPlot.R
\name{csPlot}
\alias{csPlot}
\title{Plot CRs for each CS}
\usage{
csPlot(
  cs1,
  cs2,
  group = NULL,
  data = NULL,
  ylab = "CRs",
  col = c("black", "grey"),
  legend = c("cs1", "cs2")
)
}
\arguments{
\item{cs1}{a numeric vector of values. If the \code{data} argument is
defined, it can refer to either the column index or the column name of
the data object. See \code{Details} for more information.}

\item{cs2}{a numeric vector of values. If the \code{data} argument is
defined, it can refer to either the column index or the column name of
the data object. See \code{Details} for more information.}

\item{group}{column index or name that contain the group data. See
\code{Details} for more information.}

\item{data}{numeric matrix or data frame that contains the relevant data.}

\item{ylab}{Label for the x-axis}

\item{col}{The color of the error bars to be used (either given as a numeric
vector or a character string). The length of the chosen colors should
be equal to the length of the legend names otherwise a warning is returned.}

\item{legend}{The legend names to be used. The length of the legend labels
should be the same as the length of the color string, otherwise
a warning is returned.}
}
\description{
Plot the mean of median of each CRs, for each CS
}
\details{
\code{csCompare} performs both a student t-test (using the
\code{stats::t.test} function) and a Bayesian t-test (using the
\code{BayesFactor::ttest.tstat}). In case \code{group} is not defined,
paired-samples t-tests are run. In case the \code{group} is
defined, then the csCompare first computes difference scores between the cs1
and the cs2
(i.e., cs1 - cs2).
In case the group argument is defined
but, after removal of NA's (\code{stats::na.omit}), only one group
is defined, a paired samples t-test is run.
}
\examples{
set.seed(1000)
csPlot(cs1 = rnorm(n = 100, mean = 10), cs2 = rnorm(n = 100, mean = 9))
}
\seealso{
\code{\link[stats]{t.test}}, \code{\link[BayesFactor]{ttest.tstat}}
}
