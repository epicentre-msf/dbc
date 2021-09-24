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

})

