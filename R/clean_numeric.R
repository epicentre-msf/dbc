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
#' @param dict_clean Optional dictionary of value-replacement pairs (e.g.
#'   produced by [`check_numeric`]). If provided, must include columns
#'   "variable", "value", "replacement", and, if specified as an argument, all
#'   columns specified by `vars_id`.
#'
#' If no dictionary is provided, will simply apply the conversion function to
#' all columns specified in `vars`.
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
#' clean_numeric(
#'   ll1,
#'   vars = c("age", "contacts"),
#'   dict_clean = clean_num1
#' )
#'
#' # apply standardization with as.integer() rather than default as.numeric()
#' clean_numeric(
#'   ll1,
#'   vars = c("age", "contacts"),
#'   dict_clean = clean_num1,
#'   fn = as.integer
#' )
#'
#' # apply standardization but no dictionary-based cleaning
#' clean_numeric(
#'   ll1,
#'   vars = c("age", "contacts")
#' )
#'
#' @importFrom dplyr `%>%` select filter mutate any_of all_of case_when
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data .env
#' @export clean_numeric
clean_numeric <- function(x,
                          vars,
                          vars_id = NULL,
                          dict_clean = NULL,
                          fn = as.numeric,
                          na = ".na") {

  fn <- match.fun(fn)

  # prep x
  x_prep <- x %>%
    mutate(ROWID_TEMP_ = seq_len(nrow(.)), .before = 1) %>%
    reclass_cols(cols = vars, fn = as.character)

  # pivot numeric vars to long format
  x_long <- x_prep %>%
    select(all_of("ROWID_TEMP_"), any_of(vars_id), all_of(vars)) %>%
    tidyr::pivot_longer(cols = -any_of(c("ROWID_TEMP_", vars_id)), names_to = "variable")

  # apply dictionary-specified replacements
  if (!is.null(dict_clean)) {

    test_dict(dict_clean, fn, na)

    join_cols <- c(vars_id, "variable", "value")

    dict_clean_join <- dict_clean %>%
      select(any_of(vars_id), all_of(c("variable", "value", "replacement")))

    x_long <- x_long %>%
      left_join(dict_clean_join, by = join_cols) %>%
      mutate(
        value = case_when(
          .data$replacement %in% .env$na ~ NA_character_,
          !is.na(.data$replacement) ~ .data$replacement,
          TRUE ~ .data$value
        )
      ) %>%
      select(!all_of("replacement"))
  }

  # apply conversion function (i.e. default is as.numeric())
  x_long$value <- suppressWarnings(fn(x_long$value))

  # pivot corrected numeric vars to wide form
  x_long_wide <- x_long %>%
    tidyr::pivot_wider(id_cols = all_of("ROWID_TEMP_"), names_from = "variable", values_from = "value")

  # merge corrected vars back into original dataset
  x_out <- x_prep %>%
    left_join_replace(x_long_wide, cols_match = "ROWID_TEMP_") %>%
    select(!all_of("ROWID_TEMP_"))

  # return
  return(x_out)
}


