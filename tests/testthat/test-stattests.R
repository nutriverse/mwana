# Test check: mw_stattest_ageratio() ----
testthat::test_that(
  "mw_stattest_ageratio() works as expected",
  {
    ## Sample data ----
    age <- anthro.01[["age"]]
    ager <- as.character(age)
    ## Observed results ----
    x <- mw_stattest_ageratio(age, .expectedP = 0.66)

    ## Tests ----
    testthat::expect_type(x, "list")
    testthat::expect_vector(x)
    testthat::expect_named(x, c("p", "observedR", "observedP"))
    testthat::expect_error(
      mw_stattest_ageratio(ager, .expectedP = 0.66),
      regexp = paste0(
        "`age` must be of class numeric not ", class(ager), 
        ". Please try again."
      )
    )
  }
)
