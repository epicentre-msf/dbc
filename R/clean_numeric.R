#' Clean numeric variables within a dataset based on a dictionary of
#' value-replacement pairs
#'
#' @description
#' Applies a dictionary of value-replacement pairs and a conversion function
#' (defaults to [`as.numeric`]) to clean and standardize values of numeric
#' variables. To use this approach the numeric columns of the original dataset
#' should generally be imported as type "text" or "character" so that non-valid
#' values are not automatically coerced to missing values on import.
#'
#' @inheritParams check_numeric
#'
#' @param x A data frame with one or more columns to clean
#' @param vars Names of columns within `x` to clean
#' @param dict_clean Dictionary of value-replacement pairs (e.g. produced by
#'   [`check_numeric`]). Must include columns "variable", "value",
#'   "replacement", and, if specified as an argument, all columns specified by
#'   `vars_id`.
#'
#' @return
#' The original data frame `x` but with cleaned versions of columns `vars`
#'
#' @examples
#' # load example dataset and dictionary of value-replacement pairs
#' data(ll1)
#' data(clean_num1)
#'
#' # dictionary-based corrections to numeric vars 'age' and 'contacts'
#' clean_numeric(ll1, vars = c("age", "contacts"), dict_clean = clean_num1)
#'
#' @importFrom dplyr `%>%` select filter mutate any_of all_of case_when
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data .env
#' @export clean_numeric
clean_numeric <- function(x,
                          vars,
                          vars_id = NULL,
                          dict_clean,
                          fn = as.numeric,
                          na = ".na") {

  fn <- match.fun(fn)

  # validation
  test_dict(dict_clean, fn, na)

  # prep x
  x_prep <- x %>%
    mutate(rowid_temp = seq_len(nrow(.)), .before = 1) %>%
    reclass_cols(cols = vars, fn = as.character)

  # pivot numeric vars to long format
  x_long <- x_prep %>%
    dplyr::select(.data$rowid_temp, dplyr::any_of(.env$vars_id), dplyr::all_of(.env$vars)) %>%
    tidyr::pivot_longer(cols = -dplyr::any_of(c("rowid_temp", .env$vars_id)), names_to = "variable")

  # apply dictionary-specified replacements
  join_cols <- c(vars_id, "variable", "value")

  dict_clean_join <- dict_clean %>%
    dplyr::select(dplyr::any_of(.env$vars_id), .data$variable, .data$value, .data$replacement)

  x_replace <- x_long %>%
    dplyr::left_join(dict_clean_join, by = join_cols) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        .data$replacement %in% .env$na ~ NA_character_,
        !is.na(.data$replacement) ~ .data$replacement,
        TRUE ~ .data$value
      ),
      value = suppressWarnings(fn(.data$value))
    )

  # pivot corrected numeric vars to wide form
  x_replace_wide <- x_replace %>%
    tidyr::pivot_wider(id_cols = "rowid_temp", names_from = "variable", values_from = "value")

  # merge corrected vars back into original dataset
  x_out <- x_prep %>%
    left_join_replace(x_replace_wide, cols_match = "rowid_temp") %>%
    dplyr::select(-.data$rowid_temp)

  # return
  return(x_out)
}


