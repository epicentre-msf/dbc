#' Produce a dictionary of non-valid date values within a dataset, for use in
#' subsequent data cleaning
#'
#' @description
#' The resulting cleaning dictionary can be manually reviewed to fill in
#' appropriate replacement values for each non-valid date value, or a
#' missing-value keyword indicating that the value should be converted to `NA`,
#' and then used with function [`clean_dates`].
#'
#' Similar to [`check_numeric`], values are considered 'non-valid' if they
#' cannot be coerced using a given function. The default date-coercing function
#' is [`parse_dates`], which can handle a wide variety of date formats, but the
#' user could alternatively specify a simpler function like [`as.Date`]. The
#' user may also specify additional expressions that would indicate a non-valid
#' date value. For example, the expression `date_admit > Sys.Date()` could be
#' used to check for admission dates in the future.
#'
#' @inheritParams check_numeric
#'
#' @param vars Names of date columns within `x` to check
#' @param vars_id Vector of one or more ID columns within `x` on which
#'   corrections should be conditional.
#' @param queries Optional list of expressions to check for non-valid dates. May
#'   include a `.x` selector which is a stand-in for any of the date variables
#'   specified in argument `vars`. E.g.
#' ```
#' list(
#'   date_admit > date_exit,  # admission later than exit
#'   .x > Sys.Date()          # any date in future
#' )
#' ```
#' @param dict_clean Optional dictionary of value-replacement pairs (e.g.
#'   produced by a prior run of [`check_dates`]). Must include columns
#'   "variable", "value", "replacement", and all columns specified by `vars_id`.
#' @param fn Function to parse raw date values. Defaults to [`parse_dates`]. Any
#'   value not coercible by `fn` will be flagged as a "Non-valid date".
#' @param populate_na Logical indicating whether to pre-populate column
#'   "replacement" with values specified by keyword `na`, for queries of type
#'   "Non-valid date". If most non-valid dates in `x` are non-correctable,
#'   pre-populating the keyword `na` can save time during the manual
#'   verification/correction phase. Defaults to `FALSE`.
#'
#' @return
#' Data frame representing a dictionary of non-valid values, to be used in a
#' future data cleaning step (after specifying the corresponding replacement
#' values). Columns include:
#' - columns specified in `vars_id`
#' - `variable`: column name of date variable within `x`
#' - `value`: raw date value
#' - `date`: parsed date value
#' - `replacement`: correct value that should replace a given non-valid value
#' - `query`: which query was triggered by the given raw date value (if any)
#'
#' Note that, unlike functions [`check_numeric`] and [`check_categorical`],
#' which only return rows corresponding to non-valid values, this function
#' returns all date values corresponding to any observation (i.e. row) with at
#' least one non-valid date value. This is to provide context for the non-valid
#' value and aid in making the appropriate correction.
#'
#' @examples
#' # load example dataset
#' data(ll1)
#'
#' # basic output
#' check_dates(
#'   ll1,
#'   vars = c("date_onset", "date_admit", "date_exit"),
#'   vars_id = "id"
#' )
#'
#' # add additional queries to evaluate
#' check_dates(
#'   ll1,
#'   vars = c("date_onset", "date_admit", "date_exit"),
#'   vars_id = "id",
#'   queries = list(
#'     date_onset > date_admit,
#'     date_admit > date_exit,
#'     .x > as.Date("2021-01-01")
#'   )
#' )
#'
#' @importFrom dplyr `%>%` select filter mutate any_of all_of matches bind_rows
#'   if_else left_join anti_join semi_join group_by summarize
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data .env
#' @importFrom queryr query
#' @importFrom stats setNames
#' @export check_dates
check_dates <- function(x,
                        vars,
                        vars_id,
                        queries = list(),
                        dict_clean = NULL,
                        fn = parse_dates,
                        na = ".na",
                        populate_na = FALSE) {

  fn <- match.fun(fn)

  # create temp id col
  x$rowid <- seq_len(nrow(x))
  vars_id_join <- c("rowid", vars_id)

  # pivot to long form
  x_long_raw <- x %>%
    dplyr::select(dplyr::any_of(.env$vars_id_join), dplyr::all_of(.env$vars)) %>%
    reclass_cols(cols = vars, fn = as.character) %>%
    tidyr::pivot_longer(cols = -dplyr::any_of(.env$vars_id_join), names_to = "variable")

  # apply existing dictionary-based corrections, if specified
  if (!is.null(dict_clean)) {

    # prep dict_clean
    dict_clean_std <- dict_clean %>%
      dplyr::filter(!is.na(.data$replacement)) %>%
      dplyr::select(dplyr::any_of(.env$vars_id_join), .data$variable, .data$value, .data$replacement) %>%
      dplyr::mutate(replacement = as.character(.data$replacement))

    # apply corrections
    x_long_raw <- x_long_raw %>%
      dplyr::left_join(dict_clean_std, by = c(vars_id, "variable", "value")) %>%
      dplyr::mutate(
        value = dplyr::if_else(!is.na(.data$replacement), .data$replacement, .data$value),
        value = dplyr::if_else(.data$value %in% .env$na, NA_character_, .data$value)
      ) %>%
      dplyr::select(-.data$replacement)

    x <- x_long_raw %>%
      tidyr::pivot_wider(id_cols = dplyr::any_of(.env$vars_id_join), names_from = "variable", values_from = "value") %>%
      left_join_replace(x, ., cols_match = vars_id_join)
  }

  # parse dates in wide form
  x_wide_parse <- x %>%
    reclass_cols(cols = vars, fn = fn)

  # parse dates in long-form
  x_long_parse <- x_long_raw %>%
    dplyr::mutate(date = suppressWarnings(fn(.data$value)), replacement = NA_character_)

  # parse query expressions
  queries_chr <- vapply(substitute(queries), function (x) deparse(x, width.cutoff = 500L), "")

  if (!"list" %in% queries_chr) {
    # TODO: come up with better approach here
    stop("Argument `queries` must be a list of expressions", call. = FALSE)
  }

  queries_dotx <- substitute(queries)[has_dotx(queries_chr)]
  queries_no_dotx <- substitute(queries)[!has_dotx(queries_chr) & !queries_chr %in% "list"]

  # non-valid dates
  q_nonvalid <- queryr::query(
    data = x,
    !is.na(.x) & is.na(fn(.x)),
    cols_dotx = dplyr::all_of(vars),
    cols_base = dplyr::all_of(vars_id_join)
  ) %>%
    list() %>%
    stats::setNames("Non-valid date")

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
    dplyr::select(.data$query, .data$rowid, dplyr::matches("^variable\\d")) %>%
    tidyr::pivot_longer(cols = -c(.data$query, .data$rowid), values_to = "variable") %>%
    dplyr::select(-.data$name) %>%
    dplyr::filter(!is.na(.data$variable)) %>%
    dplyr::group_by(.data$rowid, .data$variable) %>%
    dplyr::summarize(query = paste(query, collapse = "; "), .groups = "drop")

  # prep output
  x_out <- x_long_parse %>%
    dplyr::semi_join(q_full, by = vars_id_join) %>%
    dplyr::left_join(q_join, by = c("rowid", "variable")) %>%
    dplyr::select(-.data$rowid)

  # populate na
  if (populate_na) {
    x_out <- x_out %>%
      dplyr::mutate(
        replacement = dplyr::if_else(
          .data$query %in% "Non-valid date",
          .env$na,
          NA_character_
        )
      )
  }

  # return
  x_out
}



#' @noRd
has_dotx <- function(exprs) {
  vapply(exprs, function(x) ".x" %in% all.vars(parse(text = x)), FALSE)
}

