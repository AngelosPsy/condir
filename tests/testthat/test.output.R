context("Functions for statistical tests")

test_that("plots", {
  set.seed(1000)
  vdiffr::expect_doppelganger("csRobPlot",  csRobustnessPlot(cs1 = rnorm(n = 100, mean = 10), cs2 = rnorm(n = 100, mean = 9)))
})
