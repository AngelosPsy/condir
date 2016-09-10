#' Shiny app for the \code{condir} package
#'
#' @description Lanches a Shiny app for performing the core analyses included
#' in \code{condir}
#' @details
#' The function can be called without any arguments (i.e., csShine()). For the
#' interface, we used a css template available at http://getbootstrap.com.
#' @references
#' Krypotos, A.-M., Klugkist, I., & Engelhard, I. M. (submitted).Bayesian
#' Hypothesis Testing for Human Threat Conditioning Research: An introduction
#' and the condir R package.
#'
#' @seealso
#' \code{\link[condir]{csCompare}}, \code{\link[stats]{t.test}},
#' \code{\link[BayesFactor]{ttest.tstat}}
#' @export
csShine <- function (){
  shiny::runApp(system.file('app', package='condir'))
}
