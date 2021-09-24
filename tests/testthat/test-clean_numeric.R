context("clean_numeric")

test_that("clean_numeric works as expected", {

  data(ll1)
  data(ll2)
  data(clean_num1)

  # test basic
  x1 <- clean_numeric(ll1, vars = c("age", "contacts"), dict = clean_num1)
  expect_equal(dim(x1), dim(ll1))
  expect_equal(names(x1), names(ll1))
  expect_is(x1$age, "numeric")
  expect_is(x1$contacts, "numeric")

  # test arg fn
  x2 <- clean_numeric(ll1, vars = c("age", "contacts"), dict = clean_num1, fn = as.integer)
  expect_is(x2$age, "integer")
  expect_is(x2$contacts, "integer")
})

