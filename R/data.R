#' An example messy dataset to clean
#'
#' @format A data.frame with 7 rows and 10 variables:
#' \describe{
#' \item{id}{Patient identifier}
#' \item{age}{Age value}
#' \item{age_unit}{Age units}
#' \item{sex}{Patient sex}
#' \item{status}{Patient status}
#' \item{contacts}{Number of epidemiological contacts}
#' \item{date_onset}{Date of symptom onset}
#' \item{date_admit}{Date of admission to hospital}
#' \item{date_exit}{Date of exit from hospital}
#' \item{exit_status}{Patient outcome status}
#' }
"ll1"

#' An example messy dataset to clean, an extension of `ll1`
#'
#' @format A data.frame with 10 rows and 10 variables:
#' \describe{
#' \item{id}{Patient identifier}
#' \item{age}{Age value}
#' \item{age_unit}{Age units}
#' \item{sex}{Patient sex}
#' \item{status}{Patient status}
#' \item{contacts}{Number of epidemiological contacts}
#' \item{date_onset}{Date of symptom onset}
#' \item{date_admit}{Date of admission to hospital}
#' \item{date_exit}{Date of exit from hospital}
#' \item{exit_status}{Patient outcome status}
#' }
"ll2"

#' A cleaning dictionary for numeric variables in example dataset `ll1`
#'
#' @format A data.frame with 4 rows and 4 variables:
#' \describe{
#' \item{variable}{Column name within dataset}
#' \item{value}{Non-valid numeric value}
#' \item{replacement}{Replacement value for given non-valid value}
#' \item{new}{Logical indicating whether the dictionary entry is new}
#' }
"clean_num1"

#' A cleaning dictionary for categorical variables in example dataset `ll1`
#'
#' @format A data.frame with 7 rows and 4 variables:
#' \describe{
#' \item{variable}{Column name within dataset}
#' \item{value}{Non-valid numeric value}
#' \item{replacement}{Replacement value for given non-valid value}
#' \item{new}{Logical indicating whether the dictionary entry is new}
#' }
"clean_categ1"

#' A dictionary of allowed values for categorical variables in example dataset
#' `ll1`
#'
#' @format A data.frame with 13 rows and 2 variables:
#' \describe{
#' \item{variable}{Column name within dataset}
#' \item{value}{Allowed categorical values for given column}
#' }
"dict_categ1"

