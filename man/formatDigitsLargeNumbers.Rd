% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphicR.R
\name{formatDigitsLargeNumbers}
\alias{formatDigitsLargeNumbers}
\title{Function factory to be used to specify the number of digits to be used
 in large numbers. The function generates numbers as strings w/o big marks
 (US/UK commas)}
\usage{
formatDigitsLargeNumbers(digits)
}
\arguments{
\item{digits}{integer value that specifies the number of digits to be used
by the resulting function}
}
\value{
a function that will take a numeric vector as an argument and 
 returns a character vector of the numeric vector with the set number of
 digits (see ?scales::lebel_number for more info) but w/o big marks
}
\description{
Function factory to be used to specify the number of digits to be used
 in large numbers. The function generates numbers as strings w/o big marks
 (US/UK commas)
}
