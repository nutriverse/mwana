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
        .by = NULL, 
        raw_muac = FALSE
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
        "`muac` should be of class numeric not ",
        class(df$muacx), ". Try again!"
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
        "`edema` should be of class character not ",
        class(df$edemax), ". Try again!"
      )
    )
    testthat::expect_error(
      df |>
        get_estimates(
          muac = muac,
          edema = ede,
          .by = NULL
        ),
      regexp = 'Code values in `edema` must only be "y" and "n". Try again!'
    )
  }
)

## When is.null(edema) & is.null(.by)----
testthat::test_that(
  "get_estimates() works OK when edema and .by are both null",
  {
    ### Get estimates ----
    p <- anthro.02 |>
      get_estimates(
        muac = muac,
        edema = NULL,
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

## When !is.null(.by) ----
testthat::test_that(
  "get_estimates() works OK when `.by` is not null",
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
    gam_p <- 5.9
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
    testthat::expect_equal(p[[2]][1], gam_n)
    testthat::expect_equal(round(p[[3]][1] * 100, 1), gam_p)
    testthat::expect_equal(p[[4]][1], sam_n)
    testthat::expect_equal(round(p[[5]][1] * 100, 1), sam_p)
    testthat::expect_equal(p[[6]][1], mam_n)
    testthat::expect_equal(round(p[[7]][1] * 100, 1), mam_p)
  }
)

## When `raw_muac` is either `TRUE` or `FALSE` ----
testthat::test_that(
  "When get_estimates() is set to `raw_muac = TRUE`, it filters outliers
  based on `flag_muac`",
  {
    ### Observed results ----
    r <- anthro.01 |>
      mw_wrangle_age(age = age) |>
      mw_wrangle_muac(
        sex = sex,
        .recode_sex = TRUE,
        muac = muac
      ) |>
      get_estimates(
        muac = muac,
        raw_muac = TRUE
      )

    ### Tests ----
    testthat::expect_s3_class(object = r, class = "tbl_df")
    testthat::expect_no_error(object = r)
  }
)


## When `raw_muac` is either `TRUE` or `FALSE` ----
testthat::test_that(
  "When get_estimates() is set to `raw_muac = FALSE`, it filters outliers
  based on `flag_mfaz`",
  {
    ### Observed results ----
    r <- anthro.01 |>
      mw_wrangle_age(age = age) |>
      mw_wrangle_muac(
        sex = sex,
        .recode_sex = TRUE,
        age = age,
        muac = muac,
        .recode_muac = TRUE,
        .to = "cm"
      ) |>
      mutate(muac = recode_muac(muac, .to = "mm")) |>
      get_estimates(
        muac = muac,
        raw_muac = FALSE
      )

    ### Tests ----
    testthat::expect_s3_class(object = r, class = "tbl_df")
    testthat::expect_no_error(object = r)
  }
)

# Test check: mw_estimate_prevalence_screening() ----
testthat::test_that(
  "mw_estimate_prevalence_screening() works OK",
  {
    ### Get estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_screening(
        muac = muac,
        edema = edema,
        .by = province
      )

    ### Observed estimates ----
    gam_n <- 61
    gam_p <- 5.9
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
    testthat::expect_equal(p[[2]][1], gam_n)
    testthat::expect_equal(round(p[[3]][1] * 100, 1), gam_p)
    testthat::expect_equal(p[[4]][1], sam_n)
    testthat::expect_equal(round(p[[5]][1] * 100, 1), sam_p)
    testthat::expect_equal(p[[6]][1], mam_n)
    testthat::expect_equal(round(p[[7]][1] * 100, 1), mam_p)
  }
)

## When is.null(.by)
testthat::test_that(
  "mw_estimate_prevalence_screening() works OK when `.by` is null",
  {
    ### Get estimates ----
    p <- anthro.02 |>
      mw_estimate_prevalence_screening(
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
  }
)

## When an age-weighting approach is applied ----
testthat::test_that(
  "mw_estimate_prevalence_screening() applies age-weighting correctly",
  {
    ## Observed results ----
    p <- anthro.04 |>
      subset(province == "Province 2") |>
      mw_estimate_prevalence_screening()

    ## Expected results ----
    gam <- 11.2
    sam <- 2.0
    mam <- 9.2

    ## Tests ----
    testthat::expect_s3_class(p, "tbl")
    testthat::expect_equal(round(p[[3]][1] * 100, 1), gam)
    testthat::expect_equal(round(p[[1]][1] * 100, 1), sam)
    testthat::expect_equal(round(p[[2]][1] * 100, 1), mam)
  }
)

## When used on a multiple-area data set ----
testthat::test_that(
  "mw_estimate_prevalence_screening() works well on a multiple-area dataset with
    different categories of analysis_approach",
  {
    ### Get the prevalence estimates ----
    p <- anthro.04 |>
      mw_estimate_prevalence_screening(muac = muac, edema = edema, .by = province)

    ### A Province whose analysis approach is unweighted ---
    province_1 <- subset(p, province == "Province 1")

    ### A Province whose analysis approach is weighted ---
    province_2 <- subset(p, province == "Province 2")

    ### A Province whose analysis approach is add missing (NA's) ---
    province_3 <- subset(p, province == "Province 3") |>
      select(!province)

    columns_to_check <- c("gam_n", "gam_p", "sam_n", "sam_p", "mam_n", "mam_p")

    ### test ----
    testthat::expect_vector(select(p, !province), size = 3, ncol(7))
    testthat::expect_s3_class(p, "tbl")
    testthat::expect_true(all(sapply(province_3[columns_to_check], \(.) all(is.na(.)))))
    testthat::expect_false(all(sapply(province_2[columns_to_check], \(.) all(is.na(.)))))
  }
)

## When the analysis path is to throw NA's ----
testthat::test_that(
  "mw_estimate_prevalence_screening() throws NA's",
  {
    ### Get the prevalence estimates ----
    p <- anthro.04 |>
      subset(province == "Province 3") |>
      mw_estimate_prevalence_screening(muac = muac, edema = edema)

    columns_to_check <- c("gam_p", "sam_p", "mam_p")

    ### test ----
    testthat::expect_vector(p, size = 1, ncol(3))
    testthat::expect_s3_class(p, "tbl")
    testthat::expect_true(all(sapply(p[columns_to_check], \(.) all(is.na(.)))))
  }
)
