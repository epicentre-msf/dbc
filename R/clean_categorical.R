#' Clean categorical variables within a dataset based on a dictionary of
#' value-replacement pairs
#'
#' @description
#' Applies a dictionary of value-replacement pairs to clean and standardize
#' values of categorical variables. Includes options for text standardization to
#' standardize minor differences in character case, spacing, and punctuation.
#'
#' @inheritParams check_categorical
#' @inheritParams clean_numeric
#'
#' @param x A data frame with one or more columns to clean
#' @param dict_clean Optional dictionary of value-replacement pairs (e.g.
#'   produced by [`check_categorical`]). Must include columns "variable",
#'   "value", "replacement", and, if specified as an argument, all columns
#'   specified by `vars_id`.
#'
#' If no dictionary is provided, will simply standardize columns to match
#' allowed values specified in `dict_allowed`.
#'
#' @return
#' The original data frame `x` but with cleaned versions of the categorical
#' variables specified in argument `dict_allowed`
#'
#' @examples
#' # load example dataset, dictionary of allowed categorical values, and
#' # cleaning dictionary
#' data(ll1)
#' data(dict_categ1)
#' data(clean_categ1)
#'
#' # dictionary-based corrections to categorical vars
#' clean_categorical(
#'   ll1,
#'   dict_allowed = dict_categ1,
#'   dict_clean = clean_categ1
#' )
#'
#' # require exact matching, including character case
#' clean_categorical(
#'   ll1,
#'   dict_allowed = dict_categ1,
#'   dict_clean = clean_categ1,
#'   fn = identity
#' )
#'
#' # apply standardization to dict_allowed but no additional dict-based cleaning
#' clean_categorical(
#'   ll1,
#'   dict_allowed = dict_categ1
#' )
#'
#' @importFrom dplyr `%>%` select filter mutate any_of all_of case_when
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data .env
#' @export clean_categorical
clean_categorical <- function(x,
                              dict_allowed,
                              dict_clean = NULL,
                              vars_id = NULL,
                              col_allowed_var = "variable",
                              col_allowed_value = "value",
                              fn = std_text,
                              na = ".na") {

  fn <- match.fun(fn)
  vars <- intersect(unique(dict_allowed[[col_allowed_var]]), names(x))

  # prep x
  x_prep <- x %>%
    mutate(ROWID_TEMP_ = seq_len(nrow(.)), .before = 1) %>%
    reclass_cols(cols = vars, fn = as.character)

  # prep dict_allowed
  dict_allowed_std <- dict_allowed %>%
    dplyr::select(variable = all_of(col_allowed_var), value = all_of(col_allowed_value))

  # pivot vars to long format
  x_long <- x_prep %>%
    dplyr::select(all_of("ROWID_TEMP_"), any_of(vars_id), all_of(vars)) %>%
    tidyr::pivot_longer(cols = -dplyr::any_of(c("ROWID_TEMP_", vars_id)), names_to = "variable") %>%
    dplyr::mutate(value_std = fn(.data$value))

  # apply dictionary-specified replacements
  if (!is.null(dict_clean)) {

    test_dict(dict_clean, fn, na)

    join_cols <- c(vars_id, "variable", "value_std")

    dict_clean_join <- dict_clean %>%
      mutate(value_std = fn(.data$value)) %>%
      dplyr::select(dplyr::any_of(vars_id), all_of(c("variable", "value_std", "replacement"))) %>%
      unique()

    x_long <- x_long %>%
      dplyr::left_join(dict_clean_join, by = join_cols) %>%
      dplyr::mutate(
        value = dplyr::case_when(
          .data$replacement %in% .env$na ~ NA_character_,
          !is.na(.data$replacement) ~ .data$replacement,
          TRUE ~ .data$value
        )
      )
  }

  # TODO: consider option to force remaining non-valid values to NA

  # pivot corrected numeric vars to wide form
  x_long_wide <- x_long %>%
    tidyr::pivot_wider(id_cols = "ROWID_TEMP_", names_from = "variable", values_from = "value") %>%
    match_coded(dict = dict_allowed_std, fn = fn)

  # merge corrected vars back into original dataset
  x_out <- x_prep %>%
    left_join_replace(x_long_wide, cols_match = "ROWID_TEMP_") %>%
    dplyr::select(!all_of("ROWID_TEMP_"))

  # return
  return(x_out)
}

