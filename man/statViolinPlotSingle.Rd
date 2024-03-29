% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalStat.R
\name{statViolinPlotSingle}
\alias{statViolinPlotSingle}
\title{creates a ggplot object showing a violin plot}
\usage{
statViolinPlotSingle(
  data,
  column = 1,
  removeNA = TRUE,
  sampleSize = NA,
  outlineColor = "black",
  fillColor = "red",
  outlineSize = 0.5,
  outlineType = "solid",
  violinAlpha = 1,
  scale = c("area", "count", "width")[1],
  trim = TRUE,
  bandwidth = 1,
  quantiles = NULL,
  xAxis = FALSE,
  yAxis = TRUE,
  yLabel = ifelse(!is.Class(data, "data.frame"), NA, ifelse(is.character(column),
    paste(column, collapse = ""), paste(colnames(data)[column], collapse = ", "))),
  title = NA,
  vertical = FALSE,
  xDefault = FALSE,
  xLimits = c(-0.75, 0.75),
  yDefault = TRUE,
  yLimits = c(0, NA),
  ...
)
}
\arguments{
\item{data}{the data to be plotted, can be a numeric/character/etc vector or
data.frame like (or tibble etc). If it is data.frame or similar the column
argument defines which column is to be used}

\item{column}{defines which column is to be used for the boxplot.
Can be integer or character (column name), note that if both (character)
column and yLabel are defined, column is used as label for the Y-axis}

\item{removeNA}{if TRUE, the NA 'values' in the vector will be removed prior
to plotting. @note this will remove warning messages and errors}

\item{sampleSize}{allows to the use of a sample of the data to be used for
the boxplot. By default sampleSize = NA, in which case all data is used}

\item{outlineColor}{defines the color of the line around the 'violin'}

\item{fillColor}{defines the color of the 'violin' itself}

\item{outlineSize}{defines the width of the line around the 'violin'}

\item{outlineType}{defines the linetype of the line around the 'violin'}

\item{violinAlpha}{defines the alpha ('see through' value) the 'violin'}

\item{scale}{default = "area", other valid values are "count" and "width".
if "area" (default), all violins have the same area (before trimming the
tails). If "count", areas are scaled proportionally to the number of
 observations. If "width", all violins have the same maximum width.
 @note taken from ?geom_violin, there is no sense in setting this argument
 for a single violin plot}

\item{trim}{If TRUE (default), trim the tails of the violins to the range of
the data. If FALSE, don't trim the tails. @note taken from ?geom_violin}

\item{bandwidth}{defines the 'adjust' parameter, which is the adjustment of
smoothing bandwidth. 1/2 means half of default bandwidth --> makes the
outline more 'jagged'}

\item{quantiles}{draws lines at the specified quantiles, eg c(0.25,0.5,0.25).
If NULL, nothing is drawn}

\item{xAxis}{defines if the x-axis is shown}

\item{yAxis}{defines if the x-axis is shown}

\item{yLabel}{set y-axis title}

\item{title}{sets title of graph}

\item{vertical}{if TRUE, flips x- and y-axis}

\item{xDefault}{default is set to FALSE, together with xLimits this can be
used to tweak the positioning and with of the box}

\item{xLimits}{default = c(-0.75,0.75), together with xDefault this can be
used to tweak the positioning and with of the box}

\item{yDefault}{default is set to TRUE, together with yLimits, this can be
used to define the exact range of the Y-axis}

\item{yLimits}{default = c(0,NA), together with yLimits, this can be
used to define the exact range of the Y-axis}

\item{...}{can be used to pass on other arguments to graphAdjust()
(like xLimits, xExpand, etc)}
}
\value{
a ggplot object
}
\description{
creates a ggplot object showing a violin plot
}
