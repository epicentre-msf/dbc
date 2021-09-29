context("clean_dates")

test_that("clean_dates works as expected", {

  data(ll1)
  data(ll2)
  data(clean_dates1)

  # test basic
  x1 <- clean_dates(ll1, vars = c("date_onset", "date_admit", "date_exit"), vars_id = "id")
  expect_equal(dim(x1), dim(ll1))
  expect_equal(names(x1), names(ll1))
  expect_is(x1$date_onset, "Date")
  expect_is(x1$date_admit, "Date")
  expect_is(x1$date_exit, "Date")

  # test arg dict_clean
  x2 <- clean_dates(ll1, vars = c("date_onset", "date_admit", "date_exit"), vars_id = "id", dict_clean = clean_dates1)
  expect_equal(
    as.character(x2$date_exit[x2$id == "M190"]),
    clean_dates1$replacement[clean_dates1$id == "M190" & clean_dates1$variable == "date_exit"]
  )
})

