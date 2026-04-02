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
  x <- tolower(x)
  has_non_ascii <- grepl("[^\\p{ASCII}]", x, perl = TRUE)
  if (any(has_non_ascii)) {
    x[has_non_ascii] <- stringi::stri_trans_general(x[has_non_ascii], id = "Latin-ASCII")
  }
  # fmt: skip
  x <- gsub("^[[:punct:]|[:space:]]+(?=[[:alnum:]])|(?<=[[:alnum:]])[[:punct:]|[:space:]]+$", "", x, perl = TRUE)
  x <- gsub("\\s+", " ", x)
  return(x)
}
