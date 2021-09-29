context("check_dates")

test_that("check_dates works as expected", {

  data(ll1)
  data(ll2)
  data(clean_dates1)

  # test basic
  x1 <- check_dates(
    ll1,
    vars = c("date_onset", "date_admit", "date_exit"),
    vars_id = "id"
  )
  expect_s3_class(x1, "tbl_df")
  expect_setequal(names(x1), c("id", "variable", "value", "date", "replacement", "query"))

  # test arg queries
  x2 <- check_dates(
    ll1,
    vars = c("date_onset", "date_admit", "date_exit"),
    vars_id = "id",
    queries = list(date_exit > date_admit)
  )
  expect_true("date_exit > date_admit" %in% x2$query)

  # test arg dict
  x3 <- check_dates(
    ll1,
    vars = c("date_onset", "date_admit", "date_exit"),
    vars_id = "id",
    dict_clean = clean_dates1
  )
  expect_equal(nrow(x3), 0L)

  x4 <- check_dates(
    ll2,   # ll2 has additional non-valid entries
    vars = c("date_onset", "date_admit", "date_exit"),
    vars_id = "id",
    dict_clean = clean_dates1
  )
  expect_gt(nrow(x4), nrow(x3))

  # test args na and populate_na
  x5 <- check_dates(
    ll1,
    vars = c("date_onset", "date_admit", "date_exit"),
    vars_id = "id",
    populate_na = TRUE,
    na = "Missing"
  )
  expect_true("Missing" %in% x5$replacement)

})

