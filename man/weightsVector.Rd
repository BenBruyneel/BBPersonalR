% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regressionTools.R
\name{weightsVector}
\alias{weightsVector}
\title{generates vector of weighted values}
\usage{
weightsVector(weight = NULL, wvalues, infiniteValue = NA)
}
\arguments{
\item{weight}{default = NULL, no weights, function will simply return NULL
otherwsie it will return a weighted version of wvalues according to the
formula:   1/wvalues^weight .  No other transformation of wvalues possible}

\item{wvalues}{a vector (numeric/integer/...) of values to be weighted}

\item{infiniteValue}{infinite values can give problems with some plot or
calculations, default is set to NA. Set to NULL if no conversion is needed}
}
\value{
a vector of weighted values (or NULL)
}
\description{
generates vector of weighted values
}
