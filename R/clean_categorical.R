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
#' @param non_allowed_to_missing Logical indicating whether to replace values
#'   that remain non-allowed, even after cleaning and standardization, to NA.
#'   Defaults to TRUE.
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
clean_categorical <- function(
  x,
  dict_allowed,
  dict_clean = NULL,
  vars_id = NULL,
  col_allowed_var = "variable",
  col_allowed_value = "value",
  non_allowed_to_missing = TRUE,
  fn = std_text,
  na = ".na"
) {
  fn <- match.fun(fn)
  vars <- intersect(unique(dict_allowed[[col_allowed_var]]), names(x))

  # prep x
  x_prep <- x %>%
    mutate(ROWID_TEMP_ = seq_len(nrow(.)), .before = 1) %>%
    reclass_cols(cols = vars, fn = as.character)

  # prep dict_allowed
  dict_allowed_std <- dict_allowed %>%
    select(variable = all_of(col_allowed_var), value = all_of(col_allowed_value))

  # pivot vars to long format
  x_long_prep <- x_prep %>%
    select(all_of("ROWID_TEMP_"), any_of(vars_id), all_of(vars)) %>%
    tidyr::pivot_longer(cols = -any_of(c("ROWID_TEMP_", vars_id)), names_to = "variable")

  x_long_prep_distinct <- x_long_prep %>%
    select(-any_of("ROWID_TEMP_")) %>%
    distinct()

  # standardize
  unique_values <- unique(x_long_prep_distinct$value)
  value_std <- suppressWarnings(fn(unique_values))
  value_map <- stats::setNames(value_std, unique_values)

  x_long <- x_long_prep_distinct %>%
    mutate(value_std = value_map[.data$value])

  # apply dictionary-specified replacements
  if (!is.null(dict_clean)) {
    test_dict(dict_clean, fn, na)

    join_cols <- c(vars_id, "variable", "value_std")

    dict_clean_join <- dict_clean %>%
      mutate(value_std = fn(.data$value)) %>%
      select(any_of(vars_id), all_of(c("variable", "value_std", "replacement"))) %>%
      unique()

    x_long <- x_long %>%
      left_join(dict_clean_join, by = join_cols) |>
      mutate(
        value_clean = case_when(
          .data$replacement %in% .env$na ~ "REPLACE_WITH_NA_TEMP_",
          !is.na(.data$replacement) ~ .data$replacement,
          TRUE ~ .data$value
        )
      )
  } else {
    x_long$replacement <- NA_character_
    x_long$value_clean <- x_long$value
  }

  dict_allowed_std <- dict_allowed %>%
    select(
      variable = all_of(col_allowed_var),
      value_dict = all_of(col_allowed_value)
    ) %>%
    mutate(
      value_clean_std = suppressWarnings(fn(.data$value_dict))
    )

  x_long_join_dict <- x_long |>
    filter(!is.na(.data$value)) |>
    select(-any_of("value_std")) |>
    mutate(
      value_clean_std = suppressWarnings(fn(.data$value_clean))
    ) |>
    left_join(
      dict_allowed_std,
      by = c("variable", "value_clean_std")
    ) |>
    mutate(
      status = case_when(
        .data$value_clean %in% c("REPLACE_WITH_NA_TEMP_") ~ "replace_na",
        .data$value == .data$value_dict ~ "match_exact",
        is.na(.data$replacement) & !is.na(.data$value_dict) ~ "match_fuzzy",
        !is.na(.data$replacement) & !is.na(.data$value_dict) ~ "match_replacement",
        is.na(.data$value_dict) & .env$non_allowed_to_missing ~ "not_allowed_to_na",
        # fmt: skip
        !is.na(.data$replacement) & is.na(.data$value_dict) & !.env$non_allowed_to_missing ~ "not_allowed_replacement_retain",
        # fmt: skip
        is.na(.data$replacement) & is.na(.data$value_dict) & !.env$non_allowed_to_missing ~ "not_allowed_orig_retain"
      ),
      value_replace = case_when(
        # replace with NA handled separately
        .data$status %in% c("match_fuzzy", "match_replacement") ~ .data$value_dict,
        # if not allowed retain, we still want to convert to replacement (.data$value_clean)
        .data$status %in% "not_allowed_replacement_retain" ~ .data$value_clean,
        .default = NA_character_
      )
    )

  values_to_replace <- x_long_join_dict |>
    filter(.data$status %in% c("match_fuzzy", "match_replacement", "not_allowed_replacement_retain"))

  values_to_na <- x_long_join_dict |>
    filter(.data$status %in% c("replace_na", "not_allowed_to_na"))

  for (j in unique(values_to_replace$variable)) {
    values_to_replace_focal <- values_to_replace |> filter(.data$variable %in% .env$j)
    m <- match(x_prep[[j]], values_to_replace_focal$value)
    x_prep[[j]][!is.na(m)] <- values_to_replace_focal$value_replace[m[!is.na(m)]]
  }

  for (j in unique(values_to_na$variable)) {
    values_to_na_focal <- values_to_na |> filter(.data$variable %in% .env$j)
    to_replace <- x_prep[[j]] %in% values_to_na_focal$value
    x_prep[[j]][to_replace] <- NA_character_
  }

  # return
  out <- x_prep |> select(-any_of("ROWID_TEMP_"))

  return(out)
}
