context("clean_categorical")

test_that("clean_categorical works as expected", {

  data(ll1)
  data(ll2)
  data(dict_categ1)
  data(clean_categ1)

  # test basic
  x1 <- clean_categorical(ll1, dict_categ1, dict_clean = clean_categ1)
  expect_equal(dim(x1), dim(ll1))
  expect_equal(names(x1), names(ll1))
  expect_equal(nrow(check_categorical(x1, dict_categ1)), 0L)

  # test args col_allowed_var and col_allowed_value
  dict_allowed_custom <- data.frame(
    col_value = dict_categ1$value,
    col_variable = dict_categ1$variable
  )

  x2 <- clean_categorical(
    ll1,
    dict_allowed_custom,
    dict_clean = clean_categ1,
    col_allowed_var   = "col_variable",
    col_allowed_value = "col_value",
  )

  expect_equal(x1, x2)

  # test with no dictionary-based cleaning
  x3 <- clean_categorical(
    ll1,
    dict_categ1
  )
  expect_equal(
    x3$age_unit,
    c("Years", "Months", NA, NA, "Days", NA, "Years")
  )

  # test that matching is case-sensitive when fn = identity
  # i.e. age_unit "months" will not match "Months"
  x4 <- clean_categorical(
    ll1,
    dict_categ1,
    fn = identity
  )
  expect_equal(
    x4$age_unit,
    c("Years", NA, NA, NA, "Days", NA, "Years")
  )

})

