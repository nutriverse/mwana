# Test check: mw_check_ipcamn_ssreq() ----
testthat::test_that(
  "mw_check_ipcamn_ssreq() works as expected",
  {
    ## Observed results ----
    x <- mw_check_ipcamn_ssreq(
      df = anthro.01,
      cluster = cluster,
      .source = "survey"
    )

    ## Tests ----
    testthat::expect_s3_class(object = x, class = "tbl_df", exact = FALSE)
    testthat::expect_true(all(c("n_clusters", "n_obs", "meet_ipc") %in% names(x)))
    testthat::expect_error(
      mw_check_ipcamn_ssreq(
        df = anthro.01,
        cluster = weight,
        .source = "survey"
      ),
      regexp = paste0(
        "`cluster` must be of class `integer` or `character`; not ",
        shQuote(class(anthro.01$weight)), ". Please try again."
      )
    )
  }
)
