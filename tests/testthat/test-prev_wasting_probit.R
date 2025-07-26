# Test check: apply_probit_method() ----
testthat::test_that(
  "apply_probit_approach works",
  {
    ## Input data ----
    x <- anthro.03 |>
      mw_wrangle_wfhz(sex, weight, height, .recode_sex = TRUE) |>
      subset(district == "Metuge")

    ## Observed results ----
    p_gam <- apply_probit_method(x$wfhz, .status = "gam")
    p_sam <- apply_probit_method(x$wfhz, .status = "sam")

    ## Tests ----
    testthat::expect_vector(c(p_gam, p_sam), ptype = double(), size = 2)
  }
)

# Test check: estimate_probit_prevalence() ----
testthat::test_that(
  "estimate_probit_prevalence works OK",
  {
    ## Input data ----
    p <- anthro.03 |>
      mw_wrangle_wfhz(sex, weight, height, .recode_sex = TRUE) |>
      subset(district == "Metuge") |>
      estimate_probit_prevalence(.for = "wfhz")

    ## Tests ----
    testthat::expect_s3_class(p, class = "tbl", exact = FALSE)
    testthat::expect_length(p, 3)
    testthat::expect_vector(p, nrow(1), ncol(3))
  }
)

# Test check: estimate_probit_prevalence() ----
## When `.by` is not NULL ----
testthat::test_that(
  "estimate_probit_prevalence works OK",
  {
    ## Input data ----
    p <- anthro.03 |>
      mw_wrangle_wfhz(sex, weight, height, .recode_sex = TRUE) |>
      subset(district == "Metuge" | district == "Maravia") |>
      estimate_probit_prevalence(.for = "wfhz", district)

    ## Tests ----
    testthat::expect_length(p, 4)
    testthat::expect_vector(p, nrow(2))
  }
)

# Test check: estimate_probit_prevalence() ----
testthat::test_that(
  "estimate_probit_prevalence works OK with `.for` set to 'mfaz'",
  {
    ## Input data ----
    p <- mfaz.01 |>
      estimate_probit_prevalence(.for = "mfaz")

    ## Tests ----
    testthat::expect_s3_class(p, class = "tbl", exact = FALSE)
    testthat::expect_length(p, 3)
    testthat::expect_vector(p, nrow(1))
  }
)
