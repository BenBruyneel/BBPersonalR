% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalStat.R
\name{volcanoPlotPlus}
\alias{volcanoPlotPlus}
\title{generates a volcano plot of a data.frame with quantification data & 
 statisitcal significance data. Additionally the distributions of the
 quantification and the statistical data are shown via density plots (and
 optionally a qqplot)}
\usage{
volcanoPlotPlus(
  data,
  quantColumn = 1,
  statColumn = 2,
  xLabel = ifelse(is.character(quantColumn), paste(c("Log2 ", quantColumn), collapse =
    ""), paste(c("Log2 ", colnames(data)[quantColumn]), collapse = "")),
  yLabel = ifelse(is.character(statColumn), paste(c("-Log10 ", statColumn), collapse =
    ""), paste(c("-Log10 ", colnames(data)[statColumn]), collapse = "")),
  xCutoffs = c(0.5, 2),
  yCutoff = 0.05,
  title = "",
  gridLines = TRUE,
  volcanoLineMarkers = volcanoLineMarkerDedaults(),
  volcanoLineMarkerAttributes = volcanoMarkerAttributesDefaults(),
  returnData = FALSE,
  significanceColumnName = "",
  removeNonSignificantData = TRUE,
  identifierColumn = NA,
  showQQPlot = TRUE,
  widths = c(125, 875),
  heights = c(875, 125),
  ...
)
}
\arguments{
\item{data}{data.frame with at least two columns}

\item{quantColumn}{specifies which column in the data argument contains the
quantification data (can be number or character vector)}

\item{statColumn}{specifies which column in the data argument contains the
statistical significance data (can be number or character vector). Usually
these values are p-values from eg a t-test}

\item{xLabel}{defines x-axis label}

\item{yLabel}{defines y-axis label}

\item{xCutoffs}{minimum and maximum value of the quantColumn data
(before transformation). Anything outside these values and lower than
yCutoff will be marked as significant}

\item{yCutoff}{minimum value of the statColumn data. any value lower is
marked as significant (usually p-value < 0.05)}

\item{title}{specifies the title}

\item{gridLines}{if TRUE then gridlines are shown}

\item{volcanoLineMarkers}{defines where to place marker lines in the plot}

\item{volcanoLineMarkerAttributes}{defines the attributes of the marker lines}

\item{returnData}{if TRUE then a list with 2 elements is returned. The first
element is the data.frame used to generate the graph and the second element
is the graph itself}

\item{significanceColumnName}{name to give to the column specifying if a
row is significant (TRUE) or (FALSE). If not specified, then the name
"significant" will be used}

\item{removeNonSignificantData}{if TRUE, no non-significant data will be
returned (usually a smaller data.frame to return). If FALSE then all data
will be returned}

\item{identifierColumn}{specifies which columns from data (names -> character
vector) should be included in the data.frame returned}

\item{showQQPlot}{if TRUE then a qq plot of the x-axis (after transformation)
is shown in the left bottom corner}

\item{widths}{horizontal: two number (integer) vector specifying the amount
of the plot to be used for the volcanoplot and the amount for the density
plot}

\item{heights}{vertical: two number (integer) vector specifying the amount of
the plot to be used for the volcanoplot and the amount for the density plot}

\item{...}{can be used to pass on other arguments to graphAdjust()}
}
\value{
a ggplot object or a list
}
\description{
generates a volcano plot of a data.frame with quantification data & 
 statisitcal significance data. Additionally the distributions of the
 quantification and the statistical data are shown via density plots (and
 optionally a qqplot)
}
\note{
what is returned is grobtable (class gtable): this can be drawn via
 the grid.draw() function. The plot drawn before this will have to be cleared
 "manually" via clearPlot()
}
