% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{clean_dates1}
\alias{clean_dates1}
\title{A cleaning dictionary for categorical variables in example dataset \code{ll1}}
\format{
A data.frame with 12 rows and 6 variables:
\describe{
\item{id}{ID column within raw dataset}
\item{variable}{Column name within raw dataset}
\item{value}{Raw date value}
\item{date}{Parsed date value}
\item{replacement}{Replacement value for given non-valid value}
\item{query}{Which query was triggered by the given raw date value (if any)}
}
}
\usage{
clean_dates1
}
\description{
A cleaning dictionary for categorical variables in example dataset \code{ll1}
}
\keyword{datasets}
