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
#' @param queries Optional list of expressions to check for non-valid values.
#'   May include a `.x` selector which is a stand-in for any of the numeric
#'   variables specified in argument `vars`. E.g.
#' ```
#' list(
#'   age > 110,  # age greater than 110
#'   .x < 0      # any numeric value less than 0
#' )
#' ```
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
#' - `value`: non-valid value
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
#' # add custom query
#' check_numeric(ll1, c("age", "contacts"), vars_id = "id", queries = list(age > 90))
#'
#' # prepopulate column 'replacement'
#' check_numeric(ll1, c("age", "contacts"), vars_id = "id", populate_na = TRUE)
#'
#' # use dictionary of pre-specified corrections
#' check_numeric(ll1, c("age", "contacts"), dict_clean = clean_num1)
#'
#' @importFrom dplyr `%>%` select filter mutate any_of all_of matches bind_rows
#'   if_else left_join anti_join semi_join group_by summarize
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data .env
#' @importFrom queryr query
#' @importFrom stats setNames
#' @export check_numeric
check_numeric <- function(x,
                          vars,
                          vars_id = NULL,
                          queries = list(),
                          dict_clean = NULL,
                          fn = as.numeric,
                          na = ".na",
                          populate_na = FALSE,
                          return_all = FALSE) {

  fn <- match.fun(fn)

  # create temp id col
  x$ROWID_TEMP_ <- seq_len(nrow(x))
  vars_id_join <- c("ROWID_TEMP_", vars_id)

  # pivot to long form
  x_long_raw <- x %>%
    dplyr::select(dplyr::any_of(vars_id_join), dplyr::all_of(vars)) %>%
    reclass_cols(cols = vars, fn = as.character) %>%
    tidyr::pivot_longer(cols = -dplyr::any_of(vars_id_join), names_to = "variable")

  # apply existing dictionary-based corrections, if specified
  if (!is.null(dict_clean)) {

    # prep dict_clean
    dict_clean_std <- dict_clean %>%
      dplyr::filter(!is.na(.data$replacement)) %>%
      dplyr::select(dplyr::any_of(vars_id_join), all_of(c("variable", "value", "replacement"))) %>%
      dplyr::mutate(replacement = as.character(.data$replacement))

    # apply corrections
    x_long_raw <- x_long_raw %>%
      dplyr::left_join(dict_clean_std, by = c(vars_id, "variable", "value")) %>%
      dplyr::mutate(
        value = dplyr::if_else(!is.na(.data$replacement), .data$replacement, .data$value),
        value = dplyr::if_else(.data$value %in% .env$na, NA_character_, .data$value)
      )

    x <- x_long_raw %>%
      dplyr::select(!all_of("replacement")) %>%
      tidyr::pivot_wider(id_cols = dplyr::any_of(vars_id_join), names_from = "variable", values_from = "value") %>%
      left_join_replace(x, ., cols_match = vars_id_join)

  } else {
    x_long_raw$replacement <- NA_character_
  }

  # parse numeric in wide form
  x_wide_parse <- x %>%
    reclass_cols(cols = vars, fn = fn)

  # parse query expressions
  queries_chr <- vapply(substitute(queries), function (x) deparse(x, width.cutoff = 500L), "")

  if (!"list" %in% queries_chr) {
    # TODO: come up with better approach here
    stop("Argument `queries` must be a list of expressions", call. = FALSE)
  }

  queries_dotx <- substitute(queries)[has_dotx(queries_chr)]
  queries_no_dotx <- substitute(queries)[!has_dotx(queries_chr) & !queries_chr %in% "list"]

  # non-valid numeric
  q_nonvalid <- queryr::query(
    data = x,
    !is.na(.x) & is.na(suppressWarnings(fn(.x))),
    cols_dotx = dplyr::all_of(vars),
    cols_base = dplyr::all_of(vars_id_join)
  ) %>%
    list() %>%
    stats::setNames("Non-valid number")

  ## other date queries
  q_dotx <- list()
  q_no_dotx <- list()

  # queries with dotx selector
  if (length(queries_dotx) > 0) {
    for (j in seq_along(queries_dotx)) {
      q_dotx[[deparse(queries_dotx[[j]], width.cutoff = 500L)]] <- do.call(
        queryr::query,
        list(data = x_wide_parse, cond = queries_dotx[[j]], cols_dotx = vars, cols_base = vars_id_join)
      )
    }
  }

  # queries without dotx selector
  if (length(queries_no_dotx) > 0) {
    for (j in seq_along(queries_no_dotx)) {
      q_no_dotx[[deparse(queries_no_dotx[[j]], width.cutoff = 500L)]] <- do.call(
        queryr::query,
        list(data = x_wide_parse, cond = queries_no_dotx[[j]], cols_base = vars_id_join)
      )
    }
  }

  # combine all queries
  q_full <- dplyr::bind_rows(c(q_dotx, q_no_dotx, q_nonvalid), .id = "query")

  # prepare queries to join
  q_join <- q_full %>%
    dplyr::select(all_of(c("query", "ROWID_TEMP_")), dplyr::matches("^variable\\d")) %>%
    tidyr::pivot_longer(cols = !all_of(c("query", "ROWID_TEMP_")), values_to = "variable") %>%
    dplyr::select(!all_of("name")) %>%
    dplyr::filter(!is.na(.data$variable)) %>%
    dplyr::group_by(.data$ROWID_TEMP_, .data$variable) %>%
    dplyr::summarize(query = paste(.data$query, collapse = "; "), .groups = "drop")

  # prep output
  x_out <- x_long_raw %>%
    dplyr::filter(is.na(.data$replacement)) %>%
    dplyr::inner_join(q_join, by = c("ROWID_TEMP_", "variable")) %>%
    dplyr::select(!all_of("ROWID_TEMP_")) %>%
    unique() %>%
    dplyr::mutate(new = TRUE)

  # populate na
  if (populate_na) {
    x_out <- x_out %>%
      dplyr::mutate(
        replacement = dplyr::if_else(
          .data$query %in% "Non-valid number",
          .env$na,
          NA_character_
        )
      )
  }

  # add original rows of dict_clean to output
  if (return_all & !is.null(dict_clean)) {

    x_out_new <- x_out %>%
      dplyr::anti_join(dict_clean, by = c(vars_id, "variable", "value"))

    x_out <- dict_clean %>%
      dplyr::mutate(
        replacement = as.character(.data$replacement),
        new = as.logical(NA)
      ) %>%
      dplyr::bind_rows(x_out_new)
  }

  # return
  x_out
}

