#' Shiny app for the \code{condir} package
#'
#' @description Lanches a Shiny app for performing the core analyses included
#' in \code{condir}
#' @details
#' The function can be called without any arguments (i.e., csShine()). For the
#' interface, we used a css template available at http://getbootstrap.com.
#' @references
#' Krypotos, A. M., Klugkist, I., & Engelhard, I. M. (2017). 
#' Bayesian hypothesis testing for human threat conditioning research: 
#' An introduction and the condir R package. 
#' European Journal of Psychotraumatology, 8.
#'
#' @export
csShine <- function () {
  shiny::runApp(system.file('app', package = 'condir'))
}
