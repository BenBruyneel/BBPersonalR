% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalStat.R
\name{scatterBlandAltman}
\alias{scatterBlandAltman}
\title{generates a variant of the scatterplot when one needs to compare two sets
 of data (x & y): Bland-Altman / Tukey mean-difference plot}
\usage{
scatterBlandAltman(
  data,
  xColumn = 1,
  yColumn = 2,
  removeNA = FALSE,
  xLabel = "Mean",
  yLabel = "Difference",
  title = paste(c("Bland-Altman plot/Tukey mean-difference plot: ", yLabel, " vs ",
    xLabel), collapse = ""),
  xLog = FALSE,
  yLog = FALSE,
  ...
)
}
\arguments{
\item{data}{the data tp be plotted, data.frame or similar}

\item{xColumn}{specifies which column in the data argument contains the
x-data (refer to column via number or character vector (column name))}

\item{yColumn}{specifies which column in the data argument contains the
y-data (refer to column via number or character vector (column name))}

\item{removeNA}{if TRUE, the NA 'values' in the vector will be removed prior
to plotting. Note: this has consquence that ROWS will be removed when using
multiple columns with data.frame's}

\item{xLabel}{defines x-axis label}

\item{yLabel}{defines y-axis label}

\item{title}{specifies the title}

\item{xLog}{if TRUE then logarithmic scale is used for the x-axis}

\item{yLog}{if TRUE then logarithmic scale is used for the y-axis, note: this
will lead to errors if the difference is eg 0}

\item{...}{can be used to pass on other arguments to graphAdjust()}
}
\value{
a ggplot object
}
\description{
generates a variant of the scatterplot when one needs to compare two sets
 of data (x & y): Bland-Altman / Tukey mean-difference plot
}
\note{
work in progress...
}
