# date_to_quarter

test_that("errors", {
  # Does a date time value work
  expect_error(date_to_qtr(
    as.POSIXct("12/11/2017 00:16:13", format = "%m/%d/%Y %H:%M:%S", tz = "Europe/London"),
    type = "FYQ"
  ))
  # Does a vector of dates work
  expect_error(date_to_qtr(
    c(as.Date("2022-04-01", format = "%Y-%m-%d"), as.Date("2022-04-01", format = "%Y-%m-%d")),
    type = "FYQ"
  ))
})


test_that("output", {
  expect_identical(date_to_qtr(
    as.Date("2023-03-31", format = "%Y-%m-%d"),
    type = "FYQ"
  ), "2022/23 Q4")

  expect_identical(date_to_qtr(
    as.Date("2022-06-30", format = "%Y-%m-%d"),
    type = "FYQ"
  ), "2022/23 Q1")

  expect_identical(date_to_qtr(
    as.Date("2022-09-30", format = "%Y-%m-%d"),
    type = "FYQ"
  ), "2022/23 Q2")

  expect_identical(date_to_qtr(
    as.Date("2022-12-31", format = "%Y-%m-%d"),
    type = "FYQ"
  ), "2022/23 Q3")


  expect_identical(date_to_qtr(
    as.Date("2022-04-01", format = "%Y-%m-%d"),
    type = "FYQ"
  ), "2022/23 Q1")
})

#quarter_to_date
