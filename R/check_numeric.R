#' Produce a dictionary of non-valid numeric values within a dataset, for use in
#' subsequent data cleaning
#'
#' @description
#' The resulting cleaning dictionary can then be manually reviewed to fill in
#' appropriate replacement values for each non-valid numeric value, or a
#' missing-value keyword indicating that the value should be converted to `NA`.
#'
#' @param x A data frame with one or more columns to check
#' @param vars Names of columns within `x` to check
#' @param vars_id Optional vector of one or more ID columns within `x` on which
#'   corrections should be conditional.
#'
#'   If not specified the cleaning dictionary contains one entry for each unique
#'   combination of variable and non-valid value. If specified the cleaning
#'   dictionary contains one entry for each unique combination of variable,
#'   non-valid value, and ID variable.
#' @param dict_clean Optional dictionary of value-replacement pairs (e.g. from a
#'   previous run of this function). Must include columns "variable", "value",
#'   "replacement", and, if specified as an argument, all columns specified by
#'   `vars_id`.
#' @param fn Function to convert values to numeric. Defaults to [`as.numeric`].
#' @param na Keyword to use within column "replacement" for values that should
#'   be converted to `NA`. Defaults to ".na". The keyword is used to distinguish
#'   between "replacement" values that are missing because they have yet to be
#'   manually verified, and values that have been verified and really should be
#'   converted to `NA`.
#' @param populate_na Logical indicating whether to pre-populate column
#'   "replacement" with values specified by keyword `na`. If most non-valid
#'   values in `x` are non-correctable, pre-populating the keyword `na` can save
#'   time during the manual verification/correction phase. Defaults to `FALSE`.
#' @param return_all Logical indicating whether to return all non-valid values
#'   including those already specified in argument `dict_clean` (if specified)
#'   (`TRUE`), or only the new non-valid entries not already specified in
#'   `dict_clean` (`FALSE`). Defaults to `FALSE`.
#'
#' @return
#' Data frame representing a dictionary of non-valid values, to be used in a
#' future data cleaning step (after specifying the corresponding replacement
#' values). Columns include:
#' - columns specified in `vars_id`, if given
#' - `variable`: column name of variable within `x`
#' - `value`: non-valid numeric value
#' - `replacement`: correct value that should replace a given non-valid value
#' - `new`: logical indicating whether the entry is new (TRUE) or already
#' specified in argument `dict_clean` (`<NA>`)
#'
#' @examples
#' # load example dataset
#' data(ll1)
#' data(clean_num1)
#'
#' # basic output
#' check_numeric(ll1, c("age", "contacts"))
#'
#' # include id var "id"
#' check_numeric(ll1, c("age", "contacts"), vars_id = "id")
#'
#' # don't prepopulate column 'replacement'
#' check_numeric(ll1, c("age", "contacts"), vars_id = "id", populate_na = FALSE)
#'
#' # use dictionary of pre-specified corrections
#' check_numeric(ll1, c("age", "contacts"), dict_clean = clean_num1, return_all = TRUE)
#'
#' @importFrom dplyr `%>%` select filter mutate any_of all_of distinct bind_rows arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data .env
#' @export check_numeric
check_numeric <- function(x,
                          vars,
                          vars_id = NULL,
                          dict_clean = NULL,
                          fn = as.numeric,
                          na = ".na",
                          populate_na = FALSE,
                          return_all = FALSE) {


  fn <- match.fun(fn)

  # validation
  if (!is.null(dict_clean)) test_dict(dict_clean, fn, na)

  # pivot numeric vars to long format
  x_long <- x %>%
    reclass_cols(cols = .env$vars, fn = as.character) %>%
    dplyr::select(dplyr::any_of(.env$vars_id), dplyr::all_of(.env$vars)) %>%
    tidyr::pivot_longer(cols = -dplyr::any_of(.env$vars_id), names_to = "variable")

  # try converting to numeric
  x_long_std <- x_long %>%
    mutate(value_std = suppressWarnings(fn(.data$value)))

  # apply existing dictionary-based corrections, if specified
  if (!is.null(dict_clean)) {

    # prep dict_clean
    dict_clean_std <- dict_clean %>%
      dplyr::filter(!is.na(.data$replacement)) %>%
      dplyr::mutate(replacement_std = suppressWarnings(fn(.data$replacement)))

    # apply corrections
    x_long_std <- x_long_std %>%
      dplyr::left_join(dict_clean_std, by = c(vars_id, "variable", "value")) %>%
      dplyr::mutate(
        value_std = dplyr::if_else(!is.na(.data$replacement_std), .data$replacement_std, .data$value_std),
        value_std = dplyr::if_else(.data$replacement %in% .env$na, fn(NA), .data$value_std)
      ) %>%
      dplyr::select(-.data$replacement_std)
  } else {
    x_long_std$replacement <- NA_character_
  }

  # filter to non-valid and non-replaced
  x_nonvalid <- x_long_std %>%
    dplyr::filter(is.na(.data$value_std) & !is.na(.data$value) & !.data$replacement %in% .env$na)

  # prep for output
  replacement_prepopulate <- ifelse(populate_na, na, NA_character_)

  x_out <- x_nonvalid %>%
    dplyr::select(
      dplyr::any_of(.env$vars_id),
      .data$variable,
      .data$value
    ) %>%
    dplyr::arrange(.data$variable) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      replacement = .env$replacement_prepopulate,
      new = TRUE
    )

  # add original rows of dict_clean to output
  if (return_all & !is.null(dict_clean)) {
    x_out <- dict_clean %>%
      dplyr::mutate(new = as.logical(NA)) %>%
      dplyr::bind_rows(x_out)
  }

  # return
  return(x_out)
}


