context("match_coded_vec")

test_that("match_coded_vec works as expected", {

  vals_allowed <- c("Yes", "No")

  x1 <- match_coded_vec(
    c("", ""),
    allowed = vals_allowed,
    non_allowed_to_missing = TRUE
  )

  expect_equal(x1, c(NA_character_, NA_character_))

  x2 <- match_coded_vec(
    c("", ""),
    allowed = vals_allowed,
    non_allowed_to_missing = FALSE
  )

  expect_equal(x2, c("", ""))

  x3 <- match_coded_vec(
    c("y", "yes", "Yes", NA_character_),
    allowed = vals_allowed,
    non_allowed_to_missing = TRUE
  )

  expect_equal(x3, c(NA_character_, "Yes", "Yes", NA_character_))
})

