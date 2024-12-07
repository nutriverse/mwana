# Test check: rate_propof_flagged() ----
## With `.in` set to "mfaz" ----
testthat::test_that(
  "rate_propof_flagged() with `.in` set to 'mfaz' returns the
      expected output and correct rating",
  {
    ### Sample data ----
    props <- c(
      0.0, 0.0, 0.01, 0.015, 0.2, 0.015, 0.016, 0.017, 0.05, 0.06, 0.03,
      0.03, 0.04, 0.000001, 0
    )

    ### Expected results ----
    exp <- c(
      "Excellent", "Excellent", "Excellent", "Good", "Problematic",
      "Good", "Acceptable", "Acceptable", "Problematic", "Problematic",
      "Problematic", "Problematic", "Problematic", "Excellent", "Excellent"
    ) |>
      factor(levels = c("Excellent", "Good", "Acceptable", "Problematic"))

    obs <- rate_propof_flagged(props, .in = "mfaz")

    ### Tests ----
    testthat::expect_vector(
      object = obs,
      ptype = factor(
        x = c("Excellent", "Good", "Acceptable", "Problematic"),
        levels = c("Excellent", "Good", "Acceptable", "Problematic")
      ),
      size = 15
    )
    testthat::expect_equal(obs, exp)
    testthat::expect_error(
      rate_propof_flagged(as.character(props), .in = "mfaz"),
      regexp = paste0(
        "`p` must be of class double not ", 
        class(as.character(props)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      rate_propof_flagged(as.integer(props), .in = "mfaz"),
      regexp = paste0(
        "`p` must be of class double not ", class(as.integer(props)),
        ". Please try again."
      )
    )
  }
)

## With `.in` set to "wfhz" ----
testthat::test_that(
  "rate_propof_flagged() with with `.in` set to 'wfhz' returns the
      expected output and correct rating",
  {
    ### Sample data ----
    props <- c(
      0.0, 0.0, 0.01, 0.015, 0.2, 0.015, 0.016, 0.017, 0.05, 0.06, 0.03,
      0.03, 0.04, 0.000001, 0
    )

    ### Expected results ----
    exp <- c(
      "Excellent", "Excellent", "Excellent", "Excellent", "Problematic",
      "Excellent", "Excellent", "Excellent", "Good", "Acceptable",
      "Good", "Good", "Good", "Excellent", "Excellent"
    ) |>
      factor(levels = c("Excellent", "Good", "Acceptable", "Problematic"))

    ### Observed results ----
    obs <- rate_propof_flagged(props, .in = "wfhz")

    ### Tests ----
    testthat::expect_vector(
      object = obs,
      ptype = factor(
        x = c("Excellent", "Good", "Acceptable", "Problematic"),
        levels = c("Excellent", "Good", "Acceptable", "Problematic")
      ),
      size = 15
    )
    testthat::expect_equal(obs, exp)
    testthat::expect_error(
      rate_propof_flagged(as.character(props), .in = "wfhz"),
      regexp = paste0(
        "`p` must be of class double not ", 
        class(as.character(props)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      rate_propof_flagged(as.integer(props), .in = "wfhz"),
      regexp = paste0(
        "`p` must be of class double not ", class(as.integer(props)),
        ". Please try again."
      )
    )
  }
)


## With `.in` set to "raw_muac" ----
testthat::test_that(
  "rate_propof_flagged() with with `.in` set to 'raw_muac' returns the
      expected output and correct rating",
  {
    ### Sample data ----
    props <- c(
      0.0, 0.0, 0.01, 0.015, 0.2, 0.015, 0.016, 0.017, 0.05, 0.06,
      0.03, 0.03, 0.04, 0.000001, 0
    )

    ### Expected results ----
    exp <- c(
      "Excellent", "Excellent", "Excellent", "Good", "Problematic",
      "Good", "Acceptable", "Acceptable", "Problematic", "Problematic",
      "Problematic", "Problematic", "Problematic", "Excellent", "Excellent"
    ) |>
      factor(levels = c("Excellent", "Good", "Acceptable", "Problematic"))

    ### Observed results ----
    obs <- rate_propof_flagged(props, .in = "raw_muac")

    ### Tests ----
    testthat::expect_vector(
      object = obs,
      ptype = factor(
        x = c("Excellent", "Good", "Acceptable", "Problematic"),
        levels = c("Excellent", "Good", "Acceptable", "Problematic")
      ),
      size = 15
    )
    testthat::expect_equal(obs, exp)
    testthat::expect_error(
      rate_propof_flagged(as.character(props), .in = "raw_muac"),
      regexp = paste0(
        "`p` must be of class double not ", class(as.character(props)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      rate_propof_flagged(as.integer(props), .in = "raw_muac"),
      regexp = paste0(
        "`p` must be of class double not ", class(as.integer(props)),
        ". Please try again."
      )
    )
  }
)


# Test check: rate_std()
## With `.of` set "zscores" ----
testthat::test_that(
  "Rating of acceptability of standard deviation of z-scores works as expected",
  {
    ### Sample data ----
    stds <- c(
      1.253, 1.037, 0.876, 0.861, 0.8, 1.083, 1.5, 0.922, 1.269, 0.797,
      0.880, 0.853, 1.041, 1.247, 0.9
    )

    #### Expected results ----
    exp <- c(
      "Problematic", "Excellent", "Good", "Good", "Problematic",
      "Excellent", "Problematic", "Excellent", "Problematic", "Problematic",
      "Good", "Good", "Excellent", "Problematic", "Good"
    )

    #### Observed results ----
    s <- rate_std(stds, .of = "zscores")

    ### Tests ----
    testthat::expect_vector(s,
      ptype = character(),
      size = 15
    )
    testthat::expect_equal(s, exp)
    testthat::expect_error(
      rate_std(as.integer(stds), .of = "zscores"),
      regexp = paste0(
        "`sd` must be of class double not ", class(as.integer(stds)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      rate_std(as.character(stds), .of = "zscores"),
      regexp = paste0(
        "`sd` must be of class double not ", class(as.character(stds)),
        ". Please try again."
      )
    )
  }
)

## With `.of` set to "raw_muac" ----
testthat::test_that(
  "Rating of acceptability of standard deviation of raw MUAC values works as expected",
  {
    ### Sample data ----
    val <- c(12, 12, 13, 11, 13, 17, 14, 11, 16, 13, 15, 17, 17, 11, 20)

    ### Expeected results ----
    exp <- c(
      "Excellent", "Excellent", "Acceptable", "Excellent", "Acceptable",
      "Problematic", "Poor", "Excellent", "Problematic", "Acceptable",
      "Problematic", "Problematic", "Problematic", "Excellent", "Problematic"
    ) |>
      factor(levels = c("Excellent", "Acceptable", "Poor", "Problematic"))

    #### Observed results ----
    rm <- rate_std(val, .of = "raw_muac")

    ### Tests ----
    testthat::expect_vector(
      rm,
      ptype = factor(
        c("Excellent", "Acceptable", "Poor", "Problematic"),
        levels = c("Excellent", "Acceptable", "Poor", "Problematic")
      ),
      size = 15
    )
    testthat::expect_equal(rm, exp)
    testthat::expect_error(
      rate_std(as.integer(val), .of = "raw_muac"),
      regexp = paste0(
        "`sd` must be of class double not ", class(as.integer(val)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      rate_std(as.character(val), .of = "raw_muac"),
      regexp = paste0(
        "`sd` must be of class double not ", class(as.character(val)),
        ". Please try again."
      )
    )
  }
)


# Test check: rate_agesex_ratio() ----
testthat::test_that(
  "Rate of the acceptability of the age and sex ratio test's p-values works well",
  {
    ### Sample data ---
    pval <- c(
      0, 0, 0.01, 0.011, 0.2, 0.015, 0.016, 0.017, 0.05, 0.06,
      0.03, 0.03, 0.04, 0.000001, 0.07
    )

    ### Expected results ----
    exp <- c(
      "Problematic", "Problematic", "Acceptable", "Acceptable", "Excellent",
      "Acceptable", "Acceptable", "Acceptable", "Acceptable", "Good",
      "Acceptable", "Acceptable", "Acceptable", "Problematic", "Good"
    )

    ### Observed results ----
    rp <- rate_agesex_ratio(pval)

    ### Tests ----
    testthat::expect_vector(
      rp,
      ptype = character(),
      size = 15
    )
    testthat::expect_equal(rp, exp)
    testthat::expect_error(
      rate_agesex_ratio(as.character(pval)),
      regexp = paste0(
        "`p` must be of class double not ", class(as.character(pval)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      rate_agesex_ratio(as.integer(pval)),
      regexp = paste0(
        "`p` must be of class double not ", class(as.integer(pval)),
        ". Please try again."
      )
    )
  }
)

# Test check: rate_skewkurt() ----
testthat::test_that(
  "Rate of acceptability of skewness and kurtosis is as expected",
  {
    ## Sample data ----
    sk <- seq(0.1, 0.9, 0.09)

    ## Expected results ----
    exp <- c(
      "Excellent", "Excellent", "Good", "Good", "Acceptable", "Acceptable",
      "Problematic", "Problematic", "Problematic"
    ) |>
      factor(levels = c("Excellent", "Good", "Acceptable", "Problematic"))

    ## Observed results ----
    r <- rate_skewkurt(sk)

    ## Tests ----
    testthat::expect_vector(r, factor(
      levels = c("Excellent", "Good", "Acceptable", "Problematic")
    ), 9)
    testthat::expect_equal(r, exp)
    testthat::expect_error(
      rate_skewkurt(as.character(sk)),
      regexp = paste0(
        "`sk` must be of class double not ", class(as.character(sk)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      rate_skewkurt(as.integer(sk)),
      regexp = paste0(
        "`sk` must be of class double not ", class(as.integer(sk)),
        ". Please try again."
      )
    )
  }
)

# Test check: classify_overall_quality() ----
testthat::test_that(
  "rate_overall_quality() works",
  {
    ## Sample data ----
    q <- c(29, 17, 11, 5, 13, 14, 19, 26)

    ## Expected result ----
    exp <- c(
      "Problematic", "Acceptable", "Good", "Excellent", "Good", "Good",
      "Acceptable", "Problematic"
    )

    ## Observed results ----
    obs <- rate_overall_quality(q)

    ## Tests ----
    testthat::expect_true(is(obs, "factor"))
    testthat::expect_equal(as.character(obs), as.character(exp))
    testthat::expect_error(
      rate_overall_quality(as.character(q)),
      regexp = paste0(
        "`q` must be of class numeric or integer not ",
        class(as.character(q)), ". Please try again."
      )
    )
  }
)
