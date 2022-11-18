context("check_categorical")

test_that("check_categorical works as expected", {

  data(ll1)
  data(ll2)
  data(dict_categ1)

  # test basic
  x1 <- check_categorical(ll1, dict_categ1)
  expect_s3_class(x1, "tbl_df")
  expect_setequal(names(x1), c("variable", "value", "replacement", "new"))

  # test arg vars_id
  x2 <- check_categorical(ll1, dict_categ1, vars_id = "id")
  expect_setequal(names(x2), c("id", "variable", "value", "replacement", "new"))

  # test arg dict_clean
  x3 <- check_categorical(ll1, dict_categ1, dict_clean = clean_categ1)
  expect_equal(nrow(x3), 0L)

  x4 <- check_categorical(ll2, dict_categ1, dict_clean = clean_categ1) # ll2 has additional non-valid entries
  expect_gt(nrow(x4), nrow(x3))

  # test arg populate_na and na
  x5 <- check_categorical(ll1, dict_categ1, populate_na = TRUE, na = "Missing")
  expect_setequal(x5$replacement, c("Missing"))

  # test arg return_all
  x6 <- check_categorical(ll1, dict_categ1, dict_clean = clean_categ1, return_all = TRUE)
  expect_equal(nrow(x6), nrow(clean_categ1))

  x7 <- check_categorical(ll1, dict_categ1, dict_clean = clean_categ1, return_all = FALSE)
  expect_equal(nrow(x7), 0L)

  # test args col_allowed_var and col_allowed_value
  dict_allowed_custom <- data.frame(
    col_value = dict_categ1$value,
    col_variable = dict_categ1$variable
  )

  x8 <- check_categorical(
    ll1,
    dict_allowed_custom,
    dict_clean = clean_categ1,
    col_allowed_var   = "col_variable",
    col_allowed_value = "col_value",
  )

  expect_setequal(names(x8), c("variable", "value", "replacement", "new"))
  expect_equal(nrow(x8), 0L)

  # check that rows of dict_clean with no replacement are not duplicated when return_all = T
  ll <- data.frame(id = 1:2, age_unit = c("year", "Days"))
  clean_categ <- data.frame(variable = "age_unit", value = "year", replacement = NA_character_, new = as.logical(NA))

  x9 <- check_categorical(
    ll,
    dict_categ1,
    dict_clean = clean_categ,
    return_all = TRUE
  )

  expect_equal(clean_categ, x9)


})

