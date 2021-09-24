context("parse_dates")

test_that("parse_dates works as expected", {

  expect_equal(parse_dates(c("44019", "43910")), janitor::excel_numeric_to_date(c(44019, 43910)))
  expect_equal(parse_dates("13.04.1995"), as.Date("1995-04-13"))
  expect_equal(parse_dates("2000-05-05"), as.Date("2000-05-05"))
  expect_equal(parse_dates("2000-05-05 13:45:01"), as.Date("2000-05-05"))
  expect_equal(parse_dates("blah"), as.Date(NA))
})

