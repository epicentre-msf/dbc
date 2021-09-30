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
#' @param dict_clean Dictionary of value-replacement pairs (e.g. produced by
#'   [`check_categorical`]). Must include columns "variable", "value",
#'   "replacement", and, if specified as an argument, all columns specified by
#'   `vars_id`.
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
#' clean_categorical(ll1, dict_allowed = dict_categ1, dict_clean = clean_categ1)
#'
#' @importFrom dplyr `%>%` select filter mutate any_of all_of case_when
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data .env
#' @export clean_categorical
clean_categorical <- function(x,
                              dict_allowed,
                              dict_clean,
                              vars_id = NULL,
                              fn = std_text,
                              na = ".na") {


  fn <- match.fun(fn)
  vars <- intersect(unique(dict_allowed$variable), names(x))

  # validation
  test_dict(dict_clean, fn, na)

  # prep x
  x_prep <- x %>%
    mutate(rowid_temp = seq_len(nrow(.)), .before = 1) %>%
    reclass_cols(cols = vars, fn = as.character)

  # pivot vars to long format
  x_long <- x_prep %>%
    dplyr::select(.data$rowid_temp, dplyr::any_of(.env$vars_id), dplyr::all_of(.env$vars)) %>%
    match_coded(dict = dict_allowed) %>%
    tidyr::pivot_longer(cols = -dplyr::any_of(c("rowid_temp", .env$vars_id)), names_to = "variable") %>%
    dplyr::mutate(value_std = fn(.data$value))

  # apply dictionary-specified replacements
  join_cols <- c(vars_id, "variable", "value_std")

  dict_clean_join <- dict_clean %>%
    mutate(value_std = fn(.data$value)) %>%
    dplyr::select(dplyr::any_of(.env$vars_id), .data$variable, .data$value_std, .data$replacement)

  x_replace <- x_long %>%
    dplyr::left_join(dict_clean_join, by = join_cols) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        .data$replacement %in% .env$na ~ NA_character_,
        !is.na(.data$replacement) ~ .data$replacement,
        TRUE ~ .data$value
      )
    )

  # TODO: consider option to force remaining non-valid values to NA

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

