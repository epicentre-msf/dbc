
utils::globalVariables(c("."))
utils::globalVariables(c(".x"))


#' @noRd
test_dict <- function(dict, fn, na) {

  replace_std <- suppressWarnings(fn(dict$replacement))
  is_non_valid <- !is.na(dict$replacement) & is.na(replace_std) & !dict$replacement %in% na
  if (any(is_non_valid)) {
    non_valid <- unique(dict$replacement[is_non_valid])
    warning(
      "The following values of 'dict$replacement' are non-valid and will be converted to <NA>: ",
      paste_collapse(non_valid), call. = FALSE
    )
  }
}


#' @noRd
paste_collapse <- function(x, quote = TRUE, collapse = ", ") {
  if (quote) {
    out <- paste(dQuote(x, q = FALSE), collapse = collapse)
  } else {
    out <- paste(x, collapse = collapse)
  }
  return(out)
}


#' Add columns in y to x, by = cols_match, overwriting any common columns
#' @noRd
#' @importFrom dplyr `%>%` select left_join all_of
#' @importFrom rlang `!!!`
left_join_replace <- function(x, y, cols_match) {

  cols_orig_x <- names(x)
  cols_replace <- setdiff(names(y), cols_match)
  cols_keep <- setdiff(names(x), cols_replace)

  x %>%
    select(!!!cols_keep) %>%
    left_join(y, by = cols_match) %>%
    select(all_of(cols_orig_x))
}


#' @noRd
reclass_cols <- function(x, cols, fn) {
  fn <- match.fun(fn)
  for (j in cols) {
    x[[j]] <- suppressWarnings(fn(x[[j]]))
  }
  return(x)
}


#' Match dictionary-specified values
#' @noRd
match_coded <- function(x,
                        dict,
                        col_var = 1,
                        col_val = 2,
                        fn = std_text,
                        non_allowed_to_missing = TRUE) {

  fn <- match.fun(fn)

  dict_split <- split(dict[[col_val]], dict[[col_var]])
  common_cols <- intersect(names(dict_split), names(x))

  for (j in common_cols) {
    x[[j]] <- match_coded_vec(
      x[[j]],
      dict_split[[j]],
      fn = fn,
      non_allowed_to_missing = non_allowed_to_missing
    )
  }

  x
}


#' @noRd
#' @importFrom dplyr tibble mutate filter
match_coded_vec <- function(x,
                            allowed,
                            fn = std_text,
                            non_allowed_to_missing = TRUE) {

  fn <- match.fun(fn)

  x_unique <- unique(x)
  x_unique_std <-  fn(x_unique)
  allowed_std <-  fn(allowed)

  df_match <- tibble(x_unique, x_unique_std) %>%
    mutate(allowed_match = .env$allowed[match(.data$x_unique_std, .env$allowed_std)]) %>%
    filter(!is.na(.data$x_unique))

  if (!non_allowed_to_missing) {
    df_match <- df_match %>%
      filter(!is.na(.data$allowed_match))
  }

  m <- match(x, df_match$x_unique)
  out <- x
  out[!is.na(m)] <- df_match$allowed_match[m[!is.na(m)]]

  return(out)
}

