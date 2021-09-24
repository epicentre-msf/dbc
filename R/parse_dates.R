#' Parse dates
#'
#' @inheritParams lubridate::parse_date_time
#'
#' @param x A character or numeric vector of dates
#' @param convert_excel Logical indicating whether to convert Excel-encoded date
#'   values (e.g. "42370") into dates, using [janitor::excel_numeric_to_date]
#'
#' @return
#' A vector of class "Date". Values that cannot be converted to valid dates will
#' be returned as `<NA>`.
#'
#' @examples
#' x <- c("44087", "12//02/2019", "2020_05_14", "2021-01-30 14:00:04")
#' parse_dates(x)
#'
#' @importFrom lubridate as_date
#' @export parse_dates
parse_dates <- function(x,
                        convert_excel = TRUE,
                        orders = c("Ymd", "dmY", "dmy", "mdY", "Ymd HMS")) {
  x <- as.character(x)
  if (convert_excel) x <- parse_excel_dates(x)
  x <- parse_other_dates(x, orders = orders)
  suppressWarnings(lubridate::as_date(x))
}



#' @noRd
as_integer_quiet <- function(x) {
  suppressWarnings(as.integer(x))
}


#' @noRd
#' @importFrom janitor excel_numeric_to_date
parse_excel_dates <- function(x) {
  # parse character string consisting of excel date (NNNNN)
  # output will be character in "YYYY-MM-DD" format
  i <- grepl("^[[:digit:]]{4,5}$", x)
  x[i] <- as.character(janitor::excel_numeric_to_date(as_integer_quiet(x[i])))
  x
}


#' @noRd
#' @importFrom lubridate parse_date_time
parse_other_dates <- function(x, orders = c("Ymd", "dmY", "dmy", "mdY", "Ymd HMS")) {
  as.character(lubridate::parse_date_time(x, orders = orders, quiet = TRUE))
}

