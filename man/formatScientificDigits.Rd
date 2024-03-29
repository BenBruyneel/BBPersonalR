% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphicR.R
\name{formatScientificDigits}
\alias{formatScientificDigits}
\title{Function factory to be used to specify the number of digits to be used
 in numbers when using scientific notation}
\usage{
formatScientificDigits(digits)
}
\arguments{
\item{digits}{integer value that specifies the number of digits to be used
by the resulting function}
}
\value{
a function that will take a numeric vector as an argument and 
 returns a character vector of the numeric vector in scientific format with
 the set number of digits (see ?scales::lebel_scientific for more info)
}
\description{
Function factory to be used to specify the number of digits to be used
 in numbers when using scientific notation
}
\note{
this is more or less an example of a function to be used to specify
 axis-label formats with the function graphAdjust(). More complex functions
 are possible
}
