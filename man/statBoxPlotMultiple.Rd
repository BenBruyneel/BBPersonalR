% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalStat.R
\name{statBoxPlotMultiple}
\alias{statBoxPlotMultiple}
\title{creates a ggplot object showing a boxplot of multiple columns}
\usage{
statBoxPlotMultiple(
  data,
  column = 1:ncol(data),
  melted = FALSE,
  varColumn = NA,
  varToChar = FALSE,
  sampleSize = NA,
  removeNA = TRUE,
  meltOrder = NA,
  newNames = NA,
  outlineColor = "black",
  fillColor = NA,
  jitter = 0.05,
  alpha = 0.5,
  size = 3,
  shape = 16,
  jitterFill = "black",
  whiskerWidth = 0.5,
  boxWidth = 0.5,
  vertical = FALSE,
  xAxis = TRUE,
  yAxis = TRUE,
  yDefault = TRUE,
  yLimits = c(0, NA),
  xLabel = "",
  yLabel = "",
  title = "",
  showMean = TRUE,
  meanShape = 23,
  meanColor = "black",
  meanFill = "orange",
  meanSize = 5,
  legend.title = NA,
  showLegend = TRUE,
  legend.position = "bottom",
  ...
)
}
\arguments{
\item{data}{the data to be used, can be a numeric/character/etc vector or
data.frame like (or tibble etc). If it is data.frame or similar the column
argument defines which column is to be used}

\item{column}{defines which columns are to be used. Can be integer or
character (column name), note that if both (character) column and yLabel
are defined, column is used as label for the Y-axis. If not defined, then
all columns of the data.frame will be used.}

\item{melted}{boolean that defines whether the specified columns still need
to be melted into a single column for a graph. If melted = TRUE then
the argument "column" should be a single column!}

\item{varColumn}{this boolean argument is only used in case melted = TRUE.
It specifies the column to be used as variable name column}

\item{varToChar}{default FALSE, gnored if melted = FALSE. Specifies if the
variable name column should be converted into a character vector (need
discrete values)}

\item{sampleSize}{allows to the use of a sample of the data to be used for
the boxplot. By default sampleSize = NA, in which case all data is used}

\item{removeNA}{if TRUE, the NA 'values' in the vector will be removed prior
to plotting. @note this will remove warning messages and errors}

\item{meltOrder}{numeric vector which allows to define the order in which
columns should be melted onto each other. Normally the order is the same as
the column order specifoed (default NA), but this parameter allows some
extra flexibility. Be aware that columns are first melted and then
newNames is applied (if not NA)}

\item{newNames}{redefines the names of the different data columns. In
principle this could be done before this function is called, but using this
argument circumvents some issues with column names. Note that the length
of this argument (character vector) should be the same as the number of
columns, otherwise it will be ignored}

\item{outlineColor}{defines the color of the line around the box}

\item{fillColor}{defines the color of the boxes themselves. @Note: if the 
number of colors does not match the number of columns then ggplot2 default
colors will be used}

\item{jitter}{if NA, then the data points will not be shown (only outliers!),
otherwise it adds a random value to the x-values of the data points plotted.
Note: If set to 0 then they will be located on a straight line}

\item{alpha}{alpha ('see through' value) of the data (jitter) points}

\item{size}{size of the data (jitter) points}

\item{shape}{shape of the data (default = 16), see vignette
ggplot2::ggplot2-specs}

\item{jitterFill}{defines color of the jitter (single color!)}

\item{whiskerWidth}{defines the width of the whiskers (0-1)}

\item{boxWidth}{defines the width of the box (0-1)}

\item{vertical}{if TRUE, flips x- and y-axis}

\item{xAxis}{defines if the x-axis is shown}

\item{yAxis}{defines if the x-axis is shown}

\item{yDefault}{default is set to TRUE, together with yLimits, this can be
used to define the exact range of the Y-axis}

\item{yLimits}{default = c(0,NA), together with yLimits, this can be
used to define the exact range of the Y-axis}

\item{xLabel}{set x-axis title}

\item{yLabel}{set y-axis title}

\item{title}{sets title of graph}

\item{showMean}{defines if the mean value of the data should be shown}

\item{meanShape}{shape of the mean symbol (default = 23)}

\item{meanColor}{color of the line around the mean symbol}

\item{meanFill}{fill color of the shape of the mean symbol}

\item{meanSize}{size of the mean symbol}

\item{legend.title}{if not NA, then to give a non-default name to the legend}

\item{showLegend}{defines if the legend is to be shown or not}

\item{legend.position}{defines where a legend is to be placed}

\item{...}{can be used to pass on other arguments to graphAdjust()
(like xLimits, xExpand, etc)}
}
\value{
a ggplot object
}
\description{
creates a ggplot object showing a boxplot of multiple columns
}
\note{
box itself:
 bottom = 25% quantile, top = 75% quantile, middle = 50% quantile (median),
 lower whisker = 25% quantile + 1.5*IQR,
 upper whisker= 75% quantile + 1.5*IQR 
 IQR = (75% quantile) - (25% quantile) (Inter Quantile Range)
}
