context("check_numeric")

test_that("check_numeric works as expected", {

  data(ll1)
  data(ll2)
  data(clean_num1)

  # test basic
  x1 <- check_numeric(ll1, vars = c("age", "contacts"))
  expect_s3_class(x1, "tbl_df")
  expect_setequal(names(x1), c("variable", "value", "replacement", "query", "new"))

  # test arg vars_id
  x2 <- check_numeric(ll1, vars = c("age", "contacts"), vars_id = "id")
  expect_setequal(names(x2), c("id", "variable", "value", "replacement", "query", "new"))

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

  # test arg queries
  x8 <- check_numeric(
    ll1,
    vars = c("age", "contacts"),
    queries = list(age > 60),
    dict = clean_num1
  )
  expect_setequal(x8$query, "age > 60")

  # check that rows of dict_clean with no replacement are not duplicated when return_all = T
  ll <- data.frame(id = 1:2, x = c("1,2", "9"))

  clean_num <- data.frame(
    id = 1,
    variable = "x",
    value = "1,2",
    replacement = NA_character_,
    query = NA_character_,
    new = as.logical(NA)
  )

  x9 <- check_numeric(
    ll,
    vars = "x",
    vars_id = "id",
    dict_clean = clean_num,
    return_all = TRUE
  )

  expect_equal(clean_num, x9)

})

