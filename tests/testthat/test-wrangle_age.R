
### Test check: mw_wrangle_age() ----

testthat::test_that(
  "mw_wrangle_age() works as expected",
  {
    #### Sample data ----
    df <- data.frame(
      surdate = as.Date(c(
        "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01"
      )),
      birdate = as.Date(c(
        "2019-01-01", NA, "2018-03-20", "2019-11-05", "2021-04-25"
      )),
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

    #### The tests ----
    testthat::expect_type(df, "list")
    testthat::expect_vector(df[["age_days"]], size = 5)
    testthat::expect_equal(df[["age_days"]], w)
    testthat::expect_error(mw_wrangle_age(df, surdate, birdate, age_, 2))
  }
)
