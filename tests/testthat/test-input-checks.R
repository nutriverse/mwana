# Tests for input checking -----------------------------------------------------

test_that("inputs are checked for type and/or class", {
  ## Age functions (exported only) ----
  test_data <- data.frame(
    survey_date = Sys.Date(),
    dob = c(
      "2000-01-01", "2020-01-01", "01-01-2022", "Jan-01-2021",
      "01-January-2023", "January-01-2021", "20-01-01", "01-01-22"
    ),
    age = NA_character_
  )

  expect_error(
    process_age(
      df = test_data, svdate = "survey_date", birdate = "dob", age = "age"
    ),
    regexp = "dob not a Date object"
  )
})
