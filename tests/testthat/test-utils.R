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


