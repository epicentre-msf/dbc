#' Clean date variables within a dataset based on a dictionary of
#' value-replacement pairs
#'
#' @description
#' Applies a dictionary of value-replacement pairs and a conversion function
#' (defaults to [`parse_dates`]) to clean and standardize values of date
#' variables. To use this approach the date columns of the original dataset
#' should generally be imported as type "text" or "character" so that non-valid
#' values are not automatically coerced to missing values on import.
#'
#' @inheritParams check_dates
#' @inheritParams check_numeric
#'
#' @param x A data frame with one or more date columns to clean
#' @param vars Names of date columns within `x` to clean
#' @param fn Function to parse raw date values. Defaults to [`parse_dates`].
#'
#' @return
#' The original data frame `x` but with cleaned versions of the date variables
#' specified in argument `vars`
#'
#' @examples
#' # load example dataset and cleaning dictionary
#' data(ll1)
#' data(clean_dates1)
#'
#' # clean dates using only date coercion function
#' clean_dates(
#'   ll1,
#'   vars = c("date_onset", "date_admit", "date_exit"),
#'   vars_id = "id"
#' )
#'
#' # clean dates using dictionary and coercion function
#' clean_dates(
#'   ll1,
#'   vars = c("date_onset", "date_admit", "date_exit"),
#'   vars_id = "id",
#'   dict_clean = clean_dates1
#' )
#'
#' @importFrom dplyr `%>%` select filter mutate all_of if_else left_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data .env
#' @export clean_dates
clean_dates <- function(x,
                        vars,
                        vars_id,
                        dict_clean = NULL,
                        fn = parse_dates,
                        na = ".na") {

  fn <- match.fun(fn)

  # apply existing dictionary-based corrections, if specified
  if (!is.null(dict_clean)) {

    # prep x
    x <- x %>%
      mutate(ROWID_TEMP_ = seq_len(nrow(.)), .before = 1) %>%
      reclass_cols(cols = vars, fn = as.character)

    # pivot to long form
    x_long_raw <- x %>%
      select(all_of("ROWID_TEMP_"), any_of(vars_id), all_of(vars)) %>%
      tidyr::pivot_longer(cols = -all_of(c("ROWID_TEMP_", vars_id)), names_to = "variable")

    # prep dict_clean
    dict_clean_std <- dict_clean %>%
      filter(!is.na(.data$replacement)) %>%
      select(any_of(vars_id), all_of(c("variable", "value", "replacement"))) %>%
      mutate(replacement = as.character(.data$replacement))

    # apply corrections
    x_long_raw <- x_long_raw %>%
      left_join(dict_clean_std, by = c(vars_id, "variable", "value")) %>%
      mutate(
        value = if_else(!is.na(.data$replacement), .data$replacement, .data$value),
        value = if_else(.data$value %in% .env$na, NA_character_, .data$value)
      ) %>%
      select(!all_of("replacement"))

    x <- x_long_raw %>%
      tidyr::pivot_wider(id_cols = all_of(c("ROWID_TEMP_", vars_id)), names_from = "variable", values_from = "value") %>%
      left_join_replace(x, ., cols_match = c("ROWID_TEMP_", vars_id)) %>%
      select(!all_of("ROWID_TEMP_"))
  }

  # parse dates in wide form
  x_out <- x %>%
    reclass_cols(cols = vars, fn = fn)

  # return
  x_out
}

