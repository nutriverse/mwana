### Test check: get_age_months() ----

testthat::test_that(
  "calculate_age_in_months() does the job as expected",
  {
    #### Sample data ----

    df <- data.frame(
      surv_date = as.Date(
        c(
          "2024-01-05", "2024-01-05", "2024-01-05", "2024-01-08", "2024-01-08",
          "2024-01-08", "2024-01-10", "2024-01-10", "2024-01-10", "2024-01-11"
        )
      ),
      bir_date = as.Date(
        c(
          "2022-04-04", "2021-05-01", "2023-05-24", "2017-12-12", NA,
          "2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24", "2020-12-12"
        )
      ),
      svdate = c(
        "2024-01-05", "2024-01-05", "2024-01-05", "2024-01-08", "2024-01-08",
        "2024-01-08", "2024-01-10", "2024-01-10", "2024-01-10", "2024-01-11"
      ),
      birdate = c(
        "2022-04-04", "2021-05-01", "2023-05-24", "2017-12-12", NA,
        "2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24", "2020-12-12"
      )
    )

    #### Observed results ----
    x <- df |>
      mutate(
        age_mo = get_age_months(dos = surv_date, dob = bir_date)
      )

    k <- get_age_months(dos = df[["surv_date"]], dob = df[["bir_date"]])

    #### The tests ----
    testthat::expect_vector(x[["age_mo"]], size = 10)
    testthat::expect_error(get_age_months(df[["surv_date"]], df[["birdate"]]))
    testthat::expect_error(get_age_months(df[["svdate"]], df[["bir_date"]]))
    testthat::expect_true(is.numeric(k))
  }
)

## Test check: "flag_outliers()" ----
### flag_outliers with '.from' set to "zscores" ----

testthat::test_that(
  "flag_outliers() works as expected when .from = 'zscore'",
  {
    #### Sample data ----
    mfaz <- seq(-0.6, 0.9, by = 0.003) |>
      sample(size = 50, replace = TRUE)

    wrong_vector <- as.character(mfaz)

    #### Mean of mfaz ----
    x <- mean(mfaz)

    #### Expected results ----
    y <- ifelse(mfaz < (x - 3) | mfaz > (x + 3), 1, 0)

    #### Observed results ----
    z <- flag_outliers(mfaz, .from = "zscores")

    ### The tests ----
    testthat::expect_vector(z, size = 50)
    testthat::expect_equal(z, y)
    testthat::expect_true(is.numeric(z))
    testthat::expect_error(flag_outliers(wrong_vector, .from = "zscores"))
  }
)


### flag_outliers() with '.from' set to "absolute" ----
testthat::test_that(
  "flag_outliers() works as expected when .from = 'absolute'",
  {
    #### Sample data ----
    muac <- seq(80, 270, by = 4) |>
      sample(size = 20, replace = TRUE)

    wrong_vector <- as.character(muac)

    #### Expected results ----
    x <- ifelse(muac < 100 | muac > 200, 1, 0)

    #### Observed results ----
    y <- flag_outliers(muac, .from = "absolute")

    #### The tests ----
    testthat::expect_vector(y, size = 20)
    testthat::expect_equal(y, x)
    testthat::expect_true(is.numeric(y))
    testthat::expect_error(flag_outliers(wrong_vector, .from = "absolute"))
  }
)

## Test check: remove_flags() -----
### With .from set to "absolute" ----

testthat::test_that(
  "remove_flags() assign NA's when flaggs are identified",
  {
    #### Sample data ----
    muac <- c(
      88, 160, 196, 260, 204, 232, 220, 128, 204,
      84, 160, 128, 88, 156, 96, 160, 204, 220, 120, 228
    )

    wrong_vector <- as.character(muac)

    #### Expected results ----
    x <- c(
      NA, 160, 196, NA, NA, NA, NA, 128, NA, NA,
      160, 128, NA, 156, NA, 160, NA, NA, 120, NA
    )

    #### Observed results ----
    w <- remove_flags(muac, "absolute")

    #### The test ----
    testthat::expect_length(w, 20)
    testthat::expect_equal(x, w)
    testthat::expect_true(is.numeric(w))
    testthat::expect_error(remove_flags(wrong_vector, "absolute"))
  }
)

### With .from set to "zscores" ----

testthat::test_that(
  "remove_flags() assign NA's when flaggs are identified",
  {
    #### Sample data without NA's ----
    zsc <- wfhz.01$wfhz

    #### A Sample data of a wrong class for the function ----
    wrong_vector <- as.character(zsc)

    #### Observed results ----
    w <- remove_flags(zsc, "zscores")

    #### The test ----
    testthat::expect_length(w, 303)
    testthat::expect_contains(is.na(w), "TRUE")
    testthat::expect_true(is.numeric(w))
    testthat::expect_error(remove_flags(wrong_vector, "zscores"))
  }
)

## Test check: recode_muac() ----
### With .to set to "cm" ----

testthat::test_that(
  "recode_muac() works well when .to = 'cm'",
  {
    #### Sample data ----
    x <- anthro.02$muac

    #### Expected results ----
    p <- x / 10

    ### Observed results ----
    w <- recode_muac(x, .to = "cm")

    #### The test ----
    testthat::expect_vector(w, 2267)
    testthat::expect_equal(w, p)
    testthat::expect_true(is.numeric(w))
    testthat::expect_error(recode_muac(as.character(x), .to = "cm"))
  }
)

### With .to set to "mm" ----

testthat::test_that(
  "recode_muac() works well when .to = 'mm'",
  {
    #### Sample data ----
    x <- anthro.02$muac

    #### Expected results ----
    p <- x / 10
    m <- p * 10

    ### Observed results ----
    w <- recode_muac(p, .to = "mm")

    #### The test ----
    testthat::expect_vector(w, 2267)
    testthat::expect_equal(w, m)
    testthat::expect_true(is.numeric(w))
    testthat::expect_error(recode_muac(as.character(m), .to = "mm"))
  }
)
