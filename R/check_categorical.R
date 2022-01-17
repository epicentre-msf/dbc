#' Produce a dictionary of non-valid categorical values within a dataset, for
#' use in subsequent data cleaning
#'
#' @description
#' Values are compared against a user-provided dictionary specifying the allowed
#' values of each categorical variable, after text standardization to account
#' for minor differences in character case, spacing, and punctuation.
#'
#' The resulting cleaning dictionary can then be manually reviewed to fill in
#' appropriate replacement values for each non-valid categorical value, or a
#' missing-value keyword indicating that the value should be converted to `NA`.
#'
#' @inheritParams check_numeric
#' @inherit check_numeric return
#'
#' @param dict_allowed Dictionary of allowed values for each variable of
#'   interest. Must include columns for "variable" and "value" (the names of
#'   which can be modified with args `col_allowed_var` and `col_allowed_value`).
#' @param col_allowed_var Name of column in `dict_allowed` giving variable name
#'   (defaults to "variable")
#' @param col_allowed_value Name of column in `dict_allowed` giving allowed
#'   values (defaults to "value")
#' @param fn Function to standardize raw values in both the dataset and
#'   dictionary prior to comparing, to account for minor variation in character
#'   case, spacing, punctuation, etc. Defaults to [`std_text`]. To omit the
#'   standardization step can use e.g. `as.character` or an identity function
#'   `function(x) x`.
#' @param allow_na Logical indicating whether missing values should always be
#'   treated as 'allowed' even if not explicitly specified in `dict_allowed`.
#'   Defaults to `TRUE`.
#'
#' @examples
#' # load example dataset, and dictionary of allowed categorical values
#' data(ll1)
#' data(dict_categ1)
#'
#' # basic output
#' check_categorical(ll1, dict_allowed = dict_categ1)
#'
#' @importFrom dplyr `%>%` select filter mutate any_of all_of case_when if_else
#'   distinct arrange bind_rows left_join anti_join
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data .env
#' @export check_categorical
check_categorical <- function(x,
                              dict_allowed,
                              dict_clean = NULL,
                              vars_id = NULL,
                              col_allowed_var = "variable",
                              col_allowed_value = "value",
                              fn = std_text,
                              allow_na = TRUE,
                              na = ".na",
                              populate_na = FALSE,
                              return_all = FALSE) {

  fn <- match.fun(fn)
  vars <- intersect(unique(dict_allowed[[col_allowed_var]]), names(x))

  # pivot numeric vars to long format
  x_long <- x %>%
    reclass_cols(cols = vars, fn = as.character) %>%
    dplyr::select(dplyr::any_of(.env$vars_id), dplyr::all_of(.env$vars)) %>%
    tidyr::pivot_longer(cols = -dplyr::any_of(.env$vars_id), names_to = "variable")

  # standardize
  x_long_std <- x_long %>%
    dplyr::mutate(value = suppressWarnings(fn(.data$value)))

  # apply existing dictionary-based corrections, if specified
  if (!is.null(dict_clean)) {

    # prep dict
    dict_clean_std <- dict_clean %>%
      dplyr::select(dplyr::any_of(.env$vars_id), .data$variable, .data$value, .data$replacement) %>%
      dplyr::filter(!is.na(.data$replacement)) %>%
      dplyr::mutate(
        replacement = dplyr::case_when(
          replacement %in% .env$na ~ .env$na,
          TRUE ~ suppressWarnings(fn(.data$replacement))
        )
      )

    # apply corrections
    x_long_std <- x_long_std %>%
      dplyr::left_join(dict_clean_std, by = c(vars_id, "variable", "value")) %>%
      dplyr::mutate(
        value = dplyr::if_else(!is.na(.data$replacement), .data$replacement, .data$value),
        value = dplyr::if_else(.data$replacement %in% .env$na, NA_character_, .data$value)
      )
      # dplyr::select(-.data$replacement_std)
  } else {
    x_long_std$replacement <- NA_character_
  }

  # filter to non-valid and non-replaced
  dict_allowed_std <- dict_allowed %>%
    dplyr::select(variable = .env$col_allowed_var, value = .env$col_allowed_value) %>%
    dplyr::mutate(value = suppressWarnings(fn(.data$value)))

  x_nonvalid <- x_long_std %>%
    dplyr::anti_join(dict_allowed_std, by = c("variable", "value")) %>%
    dplyr::filter(is.na(.data$replacement))

  if (allow_na) {
    x_nonvalid <- x_nonvalid %>%
      filter(!is.na(.data$value))
  }

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

  # add original rows of dict to output
  if (return_all & !is.null(dict_clean)) {
    x_out <- dict_clean %>%
      dplyr::mutate(new = as.logical(NA)) %>%
      dplyr::bind_rows(x_out)
  }

  # return
  return(x_out)
}

