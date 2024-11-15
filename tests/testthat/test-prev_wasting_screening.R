# Test check: get_estimates() ----
## When is.null(.by) ----
testthat::test_that(
  "get_estimates() works OK",
  {
    ## Wrangle data ----
    df <- anthro.02 |>
      mutate(
        muacx = as.character(muac),
        edemax = as.factor(edema),
        ede = ifelse(edema == "y", "yes", 0)
      )

    ### Get estimates ----
    p <- df |>
      get_estimates(
        muac = muac,
        edema = edema,
        .by = NULL
      )

    ### Observed estimates ----
    gam_n <- 118
    gam_p <- 5.4
    sam_n <- 29
    sam_p <- 1.3
    mam_n <- 89
    mam_p <- 4.1

    ### Tests ----
    testthat::expect_s3_class(p, "tbl_df")
    testthat::expect_equal(ncol(p), 6)
    testthat::expect_equal(nrow(p), 1)
    testthat::expect_true(
      all(c("gam_n", "gam_p", "sam_n", "sam_p", "mam_n", "mam_p") %in% names(p))
    )
    testthat::expect_equal(p[[1]][1], gam_n)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), gam_p)
    testthat::expect_equal(p[[3]][1], sam_n)
    testthat::expect_equal(round(p[[4]][1] * 100, 1), sam_p)
    testthat::expect_equal(p[[5]][1], mam_n)
    testthat::expect_equal(round(p[[6]][1] * 100, 1), mam_p)
    testthat::expect_error(
      anthro.02 |>
        mutate(muac = recode_muac(muac, .to = "cm")) |>
        get_estimates(
          muac = muac,
          edema = edema,
          .by = NULL
        ),
      regexp = "MUAC values must be in millimeters. Try again!"
    )
    testthat::expect_error(
      df |>
        get_estimates(
          muac = muacx,
          edema = edema,
          .by = NULL
        ),
      regexp = paste0(
        "`muac` should be of class numeric; not ",
        shQuote(class(df$muacx)), ". Try again!"
      )
    )
    testthat::expect_error(
      df |>
        get_estimates(
          muac = muac,
          edema = edemax,
          .by = NULL
        ),
      regexp = paste0(
        "`edema` should be of class character; not ",
        shQuote(class(df$edemax)), ". Try again!"
      )
    )
    testthat::expect_error(
      df |>
        get_estimates(
          muac = muac,
          edema = ede,
          .by = NULL
        ),
      regexp = "Code values in `edema` must only be 'y' and 'n'. Try again!"
    )
  }
)

## When is.null(edema) ----
testthat::test_that(
  "get_estimates() works OK when edema is null",
  {

    ### Get estimates ----
    p <- anthro.02 |>
      get_estimates(
        muac = muac,
        .by = NULL
      )

    ### Observed estimates ----
    gam_n <- 106
    gam_p <- 4.9
    sam_n <- 16
    sam_p <- 0.7
    mam_n <- 90
    mam_p <- 4.1

    ### Tests ----
    testthat::expect_s3_class(p, "tbl_df")
    testthat::expect_equal(ncol(p), 6)
    testthat::expect_equal(nrow(p), 1)
    testthat::expect_true(
      all(c("gam_n", "gam_p", "sam_n", "sam_p", "mam_n", "mam_p") %in% names(p))
    )
    testthat::expect_equal(p[[1]][1], gam_n)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), gam_p)
    testthat::expect_equal(p[[3]][1], sam_n)
    testthat::expect_equal(round(p[[4]][1] * 100, 1), sam_p)
    testthat::expect_equal(p[[5]][1], mam_n)
    testthat::expect_equal(round(p[[6]][1] * 100, 1), mam_p)
  }
)

## When is.null(edema) ----
testthat::test_that(
  "get_estimates() works OK when edema is null",
  {

    ### Get estimates ----
    p <- anthro.02 |>
      get_estimates(
        muac = muac,
        edema = edema,
        .by = province
      )

    ### Observed estimates ----
    gam_n <- 61
    gam_p <- 6.0
    sam_n <- 19
    sam_p <- 1.8
    mam_n <- 42
    mam_p <- 4.1

    ### Tests ----
    testthat::expect_s3_class(p, "tbl_df")
    testthat::expect_equal(ncol(p), 7)
    testthat::expect_equal(nrow(p), 2)
    testthat::expect_true(
      all(c("province", "gam_n", "gam_p", "sam_n", "sam_p", "mam_n", "mam_p") %in% names(p))
    )
    testthat::expect_equal(p[[1]][1], gam_n)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), gam_p)
    testthat::expect_equal(p[[3]][1], sam_n)
    testthat::expect_equal(round(p[[4]][1] * 100, 1), sam_p)
    testthat::expect_equal(p[[5]][1], mam_n)
    testthat::expect_equal(round(p[[6]][1] * 100, 1), mam_p)
  }
)
