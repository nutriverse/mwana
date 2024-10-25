### Test check: calculate_age_in_months() ----

testthat::test_that(
  "calculate_age_in_months() does the job as expected", {
    #### Sample data ----

    df <- data.frame(
      surv_date <- as.Date(
        c("2024-01-05", "2024-01-05", "2024-01-05", "2024-01-08", "2024-01-08",
          "2024-01-08", "2024-01-10", "2024-01-10", "2024-01-10", "2024-01-11")),
      bir_date <- as.Date(
        c("2022-04-04", "2021-05-01", "2023-05-24", "2017-12-12", NA,
          "2020-12-12", "2022-04-04", "2021-05-01", "2023-05-24", "2020-12-12"))
    )

    #### Observed results ----
    df <- df |>
      mutate(
        age_mo = calculate_age_in_months(dos = surv_date, dob = bir_date)
      )

    #### The test ----
    testthat::expect_vector(df[["age_mo"]], size = 10)
    testthat::expect_true(is.double(df[["age_mo"]]))
  }
)


### Test check: mw_wrangle_age() ----

testthat::test_that(
  "mw_wrangle_age() works as expected", {

    #### Sample data ----
    df <- data.frame(
      surdate = as.Date(c(
        "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
      birdate = as.Date(c(
        "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25")),
      age = c(NA, 36, NA, NA, NA),
      bdate = c("2019-01-01", "2019-11-05", "2018-03-20", "2019-11-05", "2021-04-25"),
      age_ = as.character(c(10, 20, 13.6, 59.7, 30.1)),
      svdate = c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")
    )

    #### Expected results ----
    w <- c(1461.00, 1095.75, 1748.00, 1153.00, 616.00)

    #### Observed results ----
    df <- df |>
      mw_wrangle_age(
        dos = surdate,
        dob = birdate,
        age = age,
        .decimals = 2
        )

    #### The test ----
    testthat::expect_type(df, "list")
    testthat::expect_vector(df[["age_days"]], size = 5)
    testthat::expect_equal(df[["age_days"]], w)
    testthat::expect_error(mw_wrangle_age(df, surdate, bdate, age, 3))
    testthat::expect_error(mw_wrangle_age(df, surdate, birdate, age_, 2))
    testthat::expect_error(mw_wrangle_age(df, svdate, birdate, age, 3))
  }
)
