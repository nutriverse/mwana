# Test check: mw_stattest_ageratio() ----

testthat::test_that(
  "mw_stattest_ageratio() works as expected",
  {
    ## Observed results ----
    x <- mw_stattest_ageratio(anthro.01[["age"]], .expectedP = 0.66)

    ## Tests ----
    testthat::expect_type(x, "list")
    testthat::expect_vector(x)
    testthat::expect_named(x, c("p", "observedR", "observedP"))
    testthat::expect_error(
      mw_stattest_ageratio(as.character(anthro.01[["age"]]), .expectedP = 0.66)
    )
  }
)
