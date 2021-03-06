% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbAccess.R
\name{integerToDate}
\alias{integerToDate}
\title{converts integer date format as used by eg SQLite to R's Date format}
\usage{
integerToDate(aDate, origin = "1970-01-01")
}
\arguments{
\item{aDate}{days since origin, integer numbers to be converted Date format}

\item{origin}{character vector specifying the origin date}
}
\value{
converted dates in Date format
}
\description{
converts integer date format as used by eg SQLite to R's Date format
}
