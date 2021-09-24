context("check_numeric")

test_that("check_numeric works as expected", {

  data(ll1)
  data(ll2)
  data(clean_num1)

  # test basic
  x1 <- check_numeric(ll1, vars = c("age", "contacts"))
  expect_s3_class(x1, "tbl_df")
  expect_setequal(names(x1), c("variable", "value", "replacement", "new"))

  # test arg vars_id
  x2 <- check_numeric(ll1, vars = c("age", "contacts"), vars_id = "id")
  expect_setequal(names(x2), c("id", "variable", "value", "replacement", "new"))

  # test arg dict
  x3 <- check_numeric(ll1, vars = c("age", "contacts"), dict_clean = clean_num1)
  expect_equal(nrow(x3), 0L)

  x4 <- check_numeric(ll2, vars = c("age", "contacts"), dict_clean = clean_num1) # ll2 has additional non-valid entries
  expect_gt(nrow(x4), nrow(x3))

  # test arg na
  x5 <- check_numeric(ll1, vars = c("age", "contacts"), populate_na = TRUE, na = "Missing")
  expect_setequal(x5$replacement, c("Missing"))

  # test arg return_all
  x6 <- check_numeric(ll1, vars = c("age", "contacts"), dict = clean_num1, return_all = TRUE)
  expect_equal(nrow(x6), nrow(clean_num1))

  x7 <- check_numeric(ll1, vars = c("age", "contacts"), dict = clean_num1, return_all = FALSE)
  expect_equal(nrow(x7), 0L)
})

