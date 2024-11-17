# Test checks: define_wasting_muac() ----
## With edema ----
testthat::test_that(
  "define_wasting_muac() defines cases as it should",
  {
    ### Sample data ----
    muac_values <- c(
      123, 129, 126, 113, 130, 122, 112, 124, 128,
      121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
    )
    edema <- c(
      "n", "n", "y", "n", "n", "n", "n", "n", "n", "n", "n", "n",
      "n", "n", "n", "n", "n", "y", "y", "n"
    )

    ### Expected results ----
    exp_gam <- c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1)
    exp_sam <- c(0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0)
    exp_mam <- c(1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1)

    ### Observed results ----
    obs_gam <- define_wasting_muac(muac_values, edema, .cases = "gam")
    obs_sam <- define_wasting_muac(muac_values, edema, .cases = "sam")
    obs_mam <- define_wasting_muac(muac_values, edema, .cases = "mam")

    ### Tests ----
    testthat::expect_equal(obs_gam, exp_gam)
    testthat::expect_vector(obs_gam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_sam, exp_sam)
    testthat::expect_vector(obs_sam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_mam, exp_mam)
    testthat::expect_vector(obs_mam, ptype = numeric(), size = 20)
  }
)

## With edema set to NULL
testthat::test_that(
  "define_wasting_muac() defines cases as it should when edema is set to NULL",
  {
    ### Sample data ----
    muac_values <- c(
      123, 129, 126, 113, 130, 122, 112, 124, 128,
      121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
    )

    ### Expected results ----
    exp_gam <- c(1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1)
    exp_sam <- c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0)
    exp_mam <- c(1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1)

    ### Observed results ----
    obs_gam <- define_wasting_muac(muac_values, .cases = "gam")
    obs_sam <- define_wasting_muac(muac_values, .cases = "sam")
    obs_mam <- define_wasting_muac(muac_values, .cases = "mam")

    ### Tests ----
    testthat::expect_equal(obs_gam, exp_gam)
    testthat::expect_vector(obs_gam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_sam, exp_sam)
    testthat::expect_vector(obs_sam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_mam, exp_mam)
    testthat::expect_vector(obs_mam, ptype = numeric(), size = 20)
  }
)


# Test check: define_wasting_zscores() ----
## With edema ----
testthat::test_that(
  "define_wasting_zscores() defines cases as it should",
  {
    ### Sample data ----
    wfhz <- c(
      -0.958, -2.410, -0.232, -2.289, -3.015, -1.563, -2.773, -1.442,
      -2.652, -3.257, -2.531, -2.894, -0.595, -3.378, -1.321, -2.047,
      -0.353, -0.474, -1.200, -1.079
    )
    edema <- c(
      "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n",
      "n", "n", "n", "n", "n", "y", "y", "n"
    )

    ### Expected results ----
    exp_gam <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0)
    exp_sam <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0)
    exp_mam <- c(0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0)

    ### Observed results ----
    obs_gam <- define_wasting_zscores(zscores = wfhz, edema = edema, .cases = "gam")
    obs_sam <- define_wasting_zscores(zscores = wfhz, edema = edema, .cases = "sam")
    obs_mam <- define_wasting_zscores(zscores = wfhz, edema = edema, .cases = "mam")

    ### Tests ----
    testthat::expect_equal(obs_gam, exp_gam)
    testthat::expect_vector(obs_gam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_sam, exp_sam)
    testthat::expect_vector(obs_sam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_mam, exp_mam)
    testthat::expect_vector(obs_mam, ptype = numeric(), size = 20)
  }
)


# Test check: define_wasting_cases_whz() ----
## With edema set to NULL ----
testthat::test_that(
  "define_wasting_zscores() defines cases as it should when edema is set to NULL",
  {
    ### Sample data ----
    wfhz <- c(
      -0.958, -2.410, -0.232, -2.289, -3.015, -1.563, -2.773, -1.442,
      -2.652, -3.257, -2.531, -2.894, -0.595, -3.378, -1.321, -2.047,
      -0.353, -0.474, -1.200, -1.079
    )

    edema <- NULL

    ### Expected results ----
    exp_gam <- c(0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0)
    exp_sam <- c(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
    exp_mam <- c(0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0)

    ### Observed results ----
    obs_gam <- define_wasting_zscores(zscores = wfhz, edema, .cases = "gam")
    obs_sam <- define_wasting_zscores(zscores = wfhz, edema, .cases = "sam")
    obs_mam <- define_wasting_zscores(zscores = wfhz, edema, .cases = "mam")

    ### Tests ----
    testthat::expect_equal(obs_gam, exp_gam)
    testthat::expect_vector(obs_gam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_sam, exp_sam)
    testthat::expect_vector(obs_sam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_mam, exp_mam)
    testthat::expect_vector(obs_mam, ptype = numeric(), size = 20)
  }
)

# Test check: define_wasting_combined() ----
## With edema ----
testthat::test_that(
  "define_wasting_combined() defines cases as it should",
  {
    ### Sample data ----
    wfhz <- c(
      -0.958, -2.410, -0.232, -2.289, -3.015, -1.563, -2.773, -1.442,
      -2.652, -3.257, -2.531, -2.894, -0.595, -3.378, -1.321, -2.047,
      -0.353, -0.474, -1.200, -1.079
    )
    muac_values <- c(
      123, 129, 126, 113, 130, 122, 112, 124, 128,
      121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
    )
    edema <- c(
      "n", "n", "y", "n", "n", "n", "n", "n", "n", "n", "n", "n",
      "n", "n", "n", "n", "n", "y", "y", "n"
    )

    ### Expected results ----
    exp_cgam <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    exp_csam <- c(0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0)
    exp_cmam <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1)

    ### Observed results ----
    obs_cgam <- define_wasting_combined(
      zscores = wfhz,
      muac = muac_values,
      edema = edema,
      .cases = "cgam"
    )
    obs_csam <- define_wasting_combined(
      zscores = wfhz,
      muac = muac_values,
      edema = edema,
      .cases = "csam"
    )
    obs_cmam <- define_wasting_combined(
      zscores = wfhz,
      muac = muac_values,
      edema = edema,
      .cases = "cmam"
    )

    ### The test ----
    testthat::expect_equal(obs_cgam, exp_cgam)
    testthat::expect_vector(obs_cgam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_csam, exp_csam)
    testthat::expect_vector(obs_csam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_cmam, exp_cmam)
    testthat::expect_vector(obs_cmam, ptype = numeric(), size = 20)
  }
)

## With edema set to NULL ----
testthat::test_that(
  "define_wasting_combined() defines cases as it should when edema is set to NULL",
  {
    ### Sample data ----
    wfhz <- c(
      -0.958, -2.410, -0.232, -2.289, -3.015, -1.563, -2.773, -1.442,
      -2.652, -3.257, -2.531, -2.894, -0.595, -3.378, -1.321, -2.047,
      -0.353, -0.474, -1.200, -1.079
    )
    muac_values <- c(
      123, 129, 126, 113, 130, 122, 112, 124, 128,
      121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
    )
    edema <- NULL

    ### Expected results ----
    exp_cgam <- c(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    exp_csam <- c(0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0)
    exp_cmam <- c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1)

    ### Observed results ----
    obs_cgam <- define_wasting_combined(
      zscores = wfhz,
      muac = muac_values,
      edema = edema,
      .cases = "cgam"
    )
    obs_csam <- define_wasting_combined(
      zscores = wfhz,
      muac = muac_values,
      edema = edema,
      .cases = "csam"
    )
    obs_cmam <- define_wasting_combined(
      zscores = wfhz,
      muac = muac_values,
      edema = edema,
      .cases = "cmam"
    )

    ### The test ----
    testthat::expect_equal(obs_cgam, exp_cgam)
    testthat::expect_vector(obs_cgam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_csam, exp_csam)
    testthat::expect_vector(obs_csam, ptype = numeric(), size = 20)
    testthat::expect_equal(obs_cmam, exp_cmam)
    testthat::expect_vector(obs_cmam, ptype = numeric(), size = 20)
  }
)


# Test check: define_wasting() ----
## `.by` set to "zscores" ----
testthat::test_that(
  "define_wasting() works as expected with zscores",
  {
    ### Input data ----
    df <- wfhz.01 |>
      define_wasting(
        zscores = wfhz,
        muac = NULL,
        edema = edema,
        .by = "zscores"
      ) |>
      select(gam, sam, mam)

    ### Vectors of wrong class ----
    data <- wfhz.01
    data$x <- as.character(data$wfhz)
    data$ed <- as.factor(data$edema)

    ### Tests ----
    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_named(df, c("gam", "sam", "mam"))
    testthat::expect_vector(df$gam, size = 303)
    testthat::expect_vector(df$sam, size = 303)
    testthat::expect_vector(df$mam, size = 303)
    testthat::expect_error(
      define_wasting(
        df = data,
        zscores = x,
        muac = NULL,
        edema = edema,
        .by = "zscores"
      ),
      regexp = paste0(
        "`zscores` must be of class 'double'; not ", shQuote(class(data$x)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      define_wasting(
        df = data,
        zscores = wfhz,
        muac = NULL,
        edema = ed,
        .by = "zscores"
      ),
      regexp = paste0(
        "`edema` must be of class 'character'; not ",
        shQuote(class(data$ed)), ". Please try again."
      )
    )
  }
)

## `.by` set to "muac" ----
testthat::test_that(
  "define_wasting() works as expected with muac",
  {
    ### Input data ----
    df <- mfaz.02 |>
      define_wasting(
        muac = muac,
        edema = edema,
        .by = "muac"
      ) |>
      select(gam, sam, mam)

    ### Vectors of wrong class ----
    data <- mfaz.02
    data$m <- as.character(data$muac)
    data$ed <- as.factor(data$edema)

    ### Tests ----
    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_named(df, c("gam", "sam", "mam"))
    testthat::expect_vector(df$gam, size = 303)
    testthat::expect_vector(df$sam, size = 303)
    testthat::expect_vector(df$mam, size = 303)
    testthat::expect_error(
      define_wasting(
        df = data,
        muac = m,
        edema = edema,
        .by = "muac"
      ),
      regexp = paste0(
        "`muac` must be of class 'numeric' or 'integer'; not ",
        shQuote(class(data$m)), ". Please try again."
      )
    )
    testthat::expect_error(
      define_wasting(
        df = data,
        muac = muac,
        edema = ed,
        .by = "muac"
      ),
      regexp = paste0(
        "`edema` must be of class 'character'; not ",
        shQuote(class(data$ed)), ". Please try again."
      )
    )
  }
)


## `.by` set to "combined" ----
testthat::test_that(
  "define_wasting() works as expected with for combined",
  {
    ### Input data ----
    x <- anthro.02 |>
      define_wasting(
        zscores = wfhz,
        muac = muac,
        edema = edema,
        .by = "combined"
      ) |>
      select(cgam, csam, cmam)

    ### Wrong vectors ----
    y <- anthro.02
    y$zs <- as.character(anthro.02$wfhz)
    y$m <- as.character(anthro.02$muac)
    y$ed <- ifelse(y$edema == "n", "p", "y")
    ### Tests ----
    testthat::expect_s3_class(x, "data.frame")
    testthat::expect_named(x, c("cgam", "csam", "cmam"))
    testthat::expect_vector(x$cgam, size = 2267)
    testthat::expect_vector(x$csam, size = 2267)
    testthat::expect_vector(x$cmam, size = 2267)
    testthat::expect_error(
      define_wasting(
        df = y,
        zscores = zs,
        muac = muac,
        edema = edema,
        .by = "combined"
      ),
      regexp = paste0(
        "`zscores` must be of class 'double'; not ", shQuote(class(y$zs)),
        ". Please try again."
      )
    )
    testthat::expect_error(
      define_wasting(
        df = y,
        zscores = wfhz,
        muac = m,
        edema = edema,
        .by = "combined"
      ),
      regexp = paste0(
        "`muac` must be of class 'numeric' or 'integer'; not ",
        shQuote(class(y$m)), ". Please try again."
      )
    )
    testthat::expect_error(
      define_wasting(
        df = y,
        zscores = wfhz,
        muac = muac,
        edema = ed,
        .by = "combined"
      ),
      regexp = paste0(
        "Values in `edema` should either be 'y' or 'n'. Please try again."
      )
    )
  }
)

# Test check: def_wasting_smart_muac_tool() ----
## Edema is not NULL ----
testthat::test_that(
  "The function works as designed for",
  {
    ### Input data ----
    muac_values <- c(
      123, 129, 126, 113, 130, 122, 112, 124, 128,
      121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
    )
    edema <- c(
      "n", "n", "y", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n",
      "n", "n", "n", "y", "y", "n"
    )

    expected <- c(
      "mam", "not wasted", "sam", "sam", "not wasted", "mam", "sam", "mam",
      "not wasted", "mam", "mam", "sam", "sam", "not wasted", "mam", "not wasted",
      "mam", "sam", "sam", "mam"
    )

    ### Observed results ----
    obs <- smart_tool_case_definition(muac_values, edema)

    ## Tests ----
    testthat::expect_vector(obs, ptype = "character", size = 20)
    testthat::expect_equal(obs, expected)
  }
)

## Edema is NULL ----
testthat::test_that(
  "The function works as designed for",
  {
    ### Input data ----
    muac_values <- c(
      123, 129, 126, 113, 130, 122, 112, 124, 128,
      121, 120, 110, 114, 125, 119, 127, 117, 118, 111, 115
    )

    expected <- c(
      "mam", "not wasted", "not wasted", "sam", "not wasted", "mam", "sam", "mam",
      "not wasted", "mam", "mam", "sam", "sam", "not wasted", "mam", "not wasted",
      "mam", "mam", "sam", "mam"
    )

    ### Observed results ----
    obs <- smart_tool_case_definition(muac_values)

    ## Tests ----
    testthat::expect_vector(obs, ptype = "character", size = 20)
    testthat::expect_equal(obs, expected)
  }
)
