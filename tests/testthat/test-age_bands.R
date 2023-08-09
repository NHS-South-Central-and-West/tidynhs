# generate_age_bands

test_that("errors", {
  # Does date time work
  expect_error(generate_age_bands(age_bands = "less than five"))
})

test_that("no errors", {
  # Does a null age_bands work if common_age_bands is populated correctly
  expect_no_error(
    generate_age_bands(
      common_age_bands = 1
    )
  )
})



test_that("warnings", {
  expect_warning(
    generate_age_bands(
      age_bands = c("<5", "5-9", "10-15", "20-99", "100-119", "120+"),
      common_age_bands = "a"
    )
  )
})

#order_age_bands
