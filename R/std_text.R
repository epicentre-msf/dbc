#' Standardize text prior to matching to account for minor variation in
#' character case, spacing, punctuation, or use of accents
#'
#' @description
#' Implements the following transformations:
#' 1. standardize case (`base::tolower`)
#' 2. remove diacritic/accent characters (`stringi::stri_trans_general`)
#' 3. remove sequences of space or punctuation characters at start or end of string
#' 4. replace repeated whitespace characters with a single space
#'
#' @param x A vector of strings
#'
#' @return
#' The standardized version of `x`
#'
#' @examples
#' std_text(c("CONFIRMED", "Conf.", "confirmed"))
#' std_text(c("R\u00e9publique d\u00e9mocratique du  Congo", "Nigeria_"))
#' @importFrom dplyr `%>%`
#' @importFrom stringi stri_trans_general
#' @export std_text
std_text <- function(x) {
  x %>%
    tolower(.) %>%
    stringi::stri_trans_general(., id = "Latin-ASCII") %>%
    gsub("^[[:punct:]|[:space:]]+(?=[[:alnum:]])|(?<=[[:alnum:]])[[:punct:]|[:space:]]+$", "", ., perl = TRUE) %>%
    gsub("\\s+", " ", .)
}
