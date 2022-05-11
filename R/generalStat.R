library(dplyr)
library(ggplot2)
library(qqplotr)
library(grid)
library(gridExtra)
library(cowplot)
library(tolerance)
library(scales)
library(data.table)

#' creates a ggplot object showing a histogram
#' 
#' @param data the data tp be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  defines which column(s) is/are to be used
#' @param column defines which column(s) is/are to be used for the histogram.
#'  Can be integer or character (column name(s))
#' @param binwidth defines width of the 'bins' of the histogram, if NULL
#'  (default), then it will be set automatically (with a warning). This setting
#'  is ignored in case of statCount is set tp TRUE
#' @param bins defines the number of 'bins' of the histogram, overriden by
#'  binwidth
#' @param statCount set to TRUE if the data is not numerical
#' @param variableName sets the 'combined' name of the columns (IF there is more
#'  than one!)
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this has consquence that ROWS will be removed when using
#'  multiple columns with data.frame's
#' @param outlineColor defines the color of the line around the bars
#' @param fillColor defines the color of the bars themselves. If a multi-column
#'  data.frame is plotted, the same number as the number of columns used should
#'  be used. If not the same number, then the graph will revert to default
#'  colors of ggplot
#' @param xLabel sets x-axis title
#' @param yLabel set y-axos title
#' @param title sets title of graph, if NA then the titleDefault will be used
#' @param titleDefault will be combined with the xLabel to be used as title if
#'  title == NA
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param xDefault this defines if default x-sxis limits should be used or not,
#'  see also graphAdjust() for info
#' @param xLimits  default = c(0,NA), together with xDefault, this can be
#'  used to define the exact range of the x-axis
#' @param xSymmetric if TRUE then the range of x-axis will be adjusted to be
#'  equal on both the left and the right side of the center
#' @param xSymmetricExpand allows for padding around data (x-axis), 0.05 means
#'  5 percent extra wide x-axis range
#' @param xCentered if TRUE, the plot will be 'cemtered' around the either the
#'  mean or median x-value
#' @param xMedian if TRUE then median and mean absolute deviation (mad) are
#'  used for centering the plot along the x-axis; if FALSE then the mean and the
#'  standard deviation are used
#' @param xDeviations defines how many deviations the range of the x-axis may
#'  differ from the mean or median. Range will be either (median-xDeviations*mad
#'  ,median+xDeviations**mad) or (mean - xDeviations*sd,mean + xDeviations*sd)
#' @param showLegend defines if the legend is to be shown or not
#' @param legend.position defines where a legend is to be placed
#' @param vertical if TRUE, flips x- and y-axis
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#'  
#' @returns a ggplot object
#' @export
statHist <- function(data, column = 1, binwidth = NULL, bins = NULL,
                     statCount = FALSE,
                     variableName = "variable", removeNA = TRUE,
                     outlineColor = "white", fillColor = "red",
                     xLabel = ifelse(!is.Class(data,"data.frame"),
                                     NA,
                                     ifelse(is.character(column),
                                            paste(column, collapse = ", "),
                                            paste(colnames(data)[column],
                                                  collapse = ", "))),
                     yLabel = "Frequency", title = NA,
                     titleDefault = paste(c("Histogram of ",
                                            ifelse(!is.character(column),
                                                   "",
                                                   column)),
                                          collapse = ""), 
                     xAxis = TRUE, yAxis = TRUE,
                     xDefault = TRUE,
                     xLimits = c(0,NA),
                     xSymmetric = FALSE, xSymmetricExpand = 0.05,
                     xCentered = FALSE, xMedian = FALSE, xDeviations = 4,
                     showLegend = TRUE, legend.position = "bottom",
                     vertical = FALSE,
                     ...){
  if (identical(title,NA) & !identical(titleDefault,NA)){
    if (!identical(xLabel,NA))
      title <- paste(c(titleDefault,xLabel),collapse = "")
  }
  if (is.Class(data, "data.frame")){
    data <- data[,column]
    if (length(column) == 1){
      if (!statCount){
        if (removeNA){
          g <- ggplot2::ggplot(data = data.frame(x = data) %>% stats::na.omit(), ggplot2::aes_string("x"))
        } else {
          g <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes_string("x"))
        }
        g <- g + ggplot2::geom_histogram(binwidth = binwidth, bins = bins,
                                col = outlineColor,
                                fill = fillColor) + ggplot2::theme_classic()
      } else {
        if (removeNA){
          g <- ggplot2::ggplot(data = data.frame(x = data) %>% stats::na.omit(), ggplot2::aes_string("x"))
        } else {
          g <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes_string("x"))
        }
        suppressWarnings(
          g <- g + ggplot2::geom_histogram(col = outlineColor, fill = fillColor,
                                  stat = 'count') + ggplot2::theme_classic())
      }
    } else {
      if (!statCount){
        data <- as.data.frame(reshape2::melt(data.table::as.data.table(data),
                                   measure = 1:length(column),
                                   variable.name = variableName,
                                   value.name = "value"))
        if (removeNA){
          g <- ggplot2::ggplot(data = data %>% stats::na.omit(), ggplot2::aes_string("value"))
        } else {
          g <- ggplot2::ggplot(data = data, ggplot2::aes_string("value"))
        }
        g <- g + ggplot2::geom_histogram(binwidth = binwidth, bins = bins,
                                col = outlineColor,
                                ggplot2::aes_string(fill = variableName))
        if (length(fillColor) == length(column)){
          g <- g + ggplot2::scale_fill_manual(values = fillColor)
        }
        g <- g + ggplot2::theme_classic()
      } else {
        data <- as.data.frame(reshape2::melt(data.table::as.data.table(data),
                                   measure = 1:length(column),
                                   variable.name = variableName,
                                   value.name = "value"))
        if (removeNA){
          g <- ggplot2::ggplot(data = data %>% stats::na.omit(), ggplot2::aes_string("value"))
        } else {
          g <- ggplot2::ggplot(data = data, ggplot2::aes_string("value"))
        }
        g <- g + ggplot2::geom_histogram(binwidth = binwidth, col = outlineColor,
                                ggplot2::aes_string(fill = variableName),
                                stat = 'count')
        if (length(fillColor) == length(column)){
          g <- g + ggplot2::scale_fill_manual(values = fillColor)
        }
        g <- g + ggplot2::theme_classic()
      }
    }
  } else {
    if (!statCount){
      if (removeNA){
        g <- ggplot2::ggplot(data = data.frame(x = data) %>% stats::na.omit(), ggplot2::aes_string("x"))
      } else {
        g <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes_string("x"))
      }
      g <- g + ggplot2::geom_histogram(binwidth = binwidth, bins = bins,
                              col = outlineColor,
                              fill = fillColor) + ggplot2::theme_classic()
    } else {
      if (removeNA){
        g <- ggplot2::ggplot(data = data.frame(x = data) %>% stats::na.omit(), ggplot2::aes_string("x"))
      } else {
        g <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes_string("x"))
      }
      suppressWarnings(
        g <- g + ggplot2::geom_histogram(col = outlineColor, fill = fillColor,
                                stat = 'count') + ggplot2::theme_classic()
      )
    }
  }
  if (statCount){
    xDefault <- TRUE
  }
  if (xSymmetric){
    if (!xCentered){
      xLimits <- abs(max(c(abs(max(data,
                                   na.rm = TRUE)),
                           abs(min(data,
                                   na.rm = TRUE))),
                         na.rm = TRUE))
      if (!identical(xSymmetricExpand,NA)){
        xLimits <- xLimits * (1+xSymmetricExpand)
      }
      xLimits <- c(-xLimits,xLimits)
    } else {
      if (xMedian){
        mid <- stats::median(data, na.rm = TRUE)
        dev <- stats::mad(data, na.rm = TRUE)
      } else {
        mid <- mean(data, na.rm = TRUE)
        dev <- stats::sd(data, na.rm = TRUE)
      }
      xLimits <- c(mid - (xDeviations*dev), mid + (xDeviations*dev))
    }
    xDefault <- FALSE
  }
  g <- graphsAdjust(list(g), vertical = vertical, xLabel = xLabel,
                    xDefault = xDefault, xLimits = xLimits,
                    yLabel = yLabel, titles = title,
                    setTheme = theme_minimal_adapted(xAxis = xAxis,
                                                     yAxis = yAxis,
                                                     showLegend = showLegend,
                                                     legend.position = legend.position),
                    ...)[[1]]
  return(g)
}


#' creates a ggplot object showing a densityplot
#' 
#' @param data the data tp be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  defines which column(s) is/are to be used
#' @param column defines which column(s) is/are to be used for the densityplot.
#'  Can be integer or character (column name(s))
#' @param variableName sets the 'combined' name of the columns (IF there is more
#'  than one!)
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this has consquence that ROWS will be removed when using
#'  multiple columns with data.frame's
#' @param outlineColor defines the color of the line around the densityplot
#' @param fillColor defines the color of the densityplots themselves.
#'  If a multi-column data.frame is plotted, the same number as the number of
#'  columns used should be used. If not the same number, then the graph
#'  will revert to default colors of ggplot
#' @param alpha defines the see-through factor value of tje density plot
#' @param xLabel sets x-axis title
#' @param yLabel set y-axos title
#' @param title sets title of graph, if NA then the titleDefault will be used
#' @param titleDefault will be combined with the xLabel to be used as title if
#'  title == NA
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param xDefault default is set to TRUE, together with xLimits, this can be
#'  used to define the exact range of the X-axis
#' @param xLimits  default = c(0,NA), together with xDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param xSymmetric if TRUE then the range of x-axis will be adjusted to be
#'  equal on both the left and the right side of the center (transformed x = 0)
#' @param xSymmetricExpand allows for padding around data (x-axis), 0.05 means
#'  5 percent (pre-transformation) extra wide x-axis range
#' @param xCentered if TRUE, the plot will be 'cemtered' around the either the
#'  mean or median x-value
#' @param xMedian if TRUE then median and mean absolute deviation (mad) are
#'  used for centering the plot along the x-axis; if FALSE then the mean and tge
#'  standard deviation are used
#' @param xDeviations defines how many deviations the range of the x-axis may
#'  differ from the mean or median. Range will be either (median-xDeviations*mad
#'  ,median+xDeviations**mad) or (mean - xDeviations*sd,mean + xDeviations*sd)
#' @param gridLines if TRUE then gridlines are shown
#' @param gridLinesX if TRUE then vertical gridlines are shown (set gridLines to
#'  FALSE when using this)
#' @param gridLinesY if TRUE then horizontal gridlines are shown (set gridLines
#'  to FALSE when using this)
#' @param showLegend defines if the legend is to be shown or not
#' @param legend.position defines where a legend is to be placed
#' @param vertical if TRUE, flips x- and y-axis
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#'  
#' @returns a ggplot object
#' @export
statDensity <- function(data, column = 1,
                        outlineColor = "black", fillColor = "red", alpha = 1,
                        variableName = "variable",
                        vertical = FALSE,
                        xLabel = ifelse(!is.Class(data,"data.frame"),
                                        NA,
                                        ifelse(is.character(column),
                                               paste(column, collapse = ""),
                                               paste(colnames(data)[column],
                                                     collapse = ", "))),
                        yLabel = "Probability Density", title = NA,
                        titleDefault = paste(c("Distribution of ",
                                               ifelse(!is.character(column),
                                                      "",
                                                      column)),
                                             collapse = ""), 
                        removeNA = TRUE,
                        xAxis = TRUE, yAxis = TRUE,
                        xDefault = TRUE,
                        xLimits = c(0,NA),
                        xSymmetric = FALSE, xSymmetricExpand = 0.05,
                        xCentered = FALSE, xMedian = FALSE, xDeviations = 4,
                        gridLines = TRUE,
                        gridLinesX = TRUE,
                        gridLinesY = TRUE,
                        showLegend = TRUE, legend.position = "bottom",
                        ...){
  if (identical(title,NA) & !identical(titleDefault,NA)){
    if (!identical(xLabel,NA))
      title <- paste(c(titleDefault,xLabel),collapse = "")
  }
  if (is.Class(data, "data.frame")){
    data <- data[,column]
    if (length(column) == 1){
      if (removeNA){
        g <- ggplot2::ggplot(data = data.frame(x = data) %>% stats::na.omit(), ggplot2::aes_string("x"))
      } else {
        g <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes_string("x"))
      }
      g <- g + ggplot2::geom_density(col = outlineColor, fill = fillColor,
                            alpha = alpha) + ggplot2::theme_classic()
    } else {
      data <- as.data.frame(reshape2::melt(data.table::as.data.table(data),
                                 measure = 1:length(column),
                                 variable.name = variableName,
                                 value.name = "value"))
      if (removeNA){
        g <- ggplot2::ggplot(data = data %>% stats::na.omit(), ggplot2::aes_string("value"))
      } else {
        g <- ggplot2::ggplot(data = data, ggplot2::aes_string("value"))
      }
      g <- g + ggplot2::geom_density(col = outlineColor, alpha = alpha,
                            ggplot2::aes_string(fill = variableName))
      if (length(fillColor) == length(column)){
        g <- g + ggplot2::scale_fill_manual(values = fillColor)
      }
      g <- g + ggplot2::theme_classic()
    }
  } else {
    g <- ggplot2::ggplot(data = data.frame(x = data) %>% stats::na.omit(), ggplot2::aes_string("x")) +
      ggplot2::geom_density(col = outlineColor,
                   fill = fillColor, alpha = alpha) + ggplot2::theme_classic()
  }
  if (xSymmetric){
    if (!xCentered){
      xLimits <- abs(max(c(abs(max(data,
                                   na.rm = TRUE)),
                           abs(min(data,
                                   na.rm = TRUE))),
                         na.rm = TRUE))
      if (!identical(xSymmetricExpand,NA)){
        xLimits <- xLimits * (1+xSymmetricExpand)
      }
      xLimits <- c(-xLimits,xLimits)
    } else {
      if (xMedian){
        mid <- stats::median(data, na.rm = TRUE)
        dev <- stats::mad(data, na.rm = TRUE)
      } else {
        mid <- mean(data, na.rm = TRUE)
        dev <- stats::sd(data, na.rm = TRUE)
      }
      xLimits <- c(mid - (xDeviations*dev), mid + (xDeviations*dev))
    }
    xDefault <- FALSE
  }
  g <- graphsAdjust(list(g),
                    vertical = vertical, xLabel = xLabel,
                    xDefault = xDefault, xLimits = xLimits,
                    yLabel = yLabel, titles = title,
                    setTheme = theme_minimal_adapted(xAxis = xAxis,
                                                     yAxis = yAxis,
                                                     showLegend = showLegend,
                                                     legend.position = legend.position,
                                                     gridLines = gridLines,
                                                     gridLinesX = gridLinesX,
                                                     gridLinesY = gridLinesY),
                    ...)[[1]]
  return(g)
}

#' creates a ggplot object showing a violin plot
#' 
#' @param data the data to be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which column is to be used for the boxplot.
#'  Can be integer or character (column name), note that if both (character)
#'  column and yLabel are defined, column is used as label for the Y-axis
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this will remove warning messages and errors 
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param outlineColor defines the color of the line around the 'violin'
#' @param fillColor defines the color of the 'violin' itself
#' @param outlineSize defines the width of the line around the 'violin'
#' @param outlineType defines the linetype of the line around the 'violin'
#' @param violinAlpha defines the alpha ('see through' value) the 'violin'
#' @param scale default = "area", other valid values are "count" and "width".
#'  if "area" (default), all violins have the same area (before trimming the
#'  tails). If "count", areas are scaled proportionally to the number of
#'   observations. If "width", all violins have the same maximum width.
#'   @note taken from ?geom_violin, there is no sense in setting this argument
#'   for a single violin plot
#' @param trim If TRUE (default), trim the tails of the violins to the range of
#'  the data. If FALSE, don't trim the tails. @note taken from ?geom_violin
#' @param bandwidth defines the 'adjust' parameter, which is the adjustment of
#'  smoothing bandwidth. 1/2 means half of default bandwidth --> makes the
#'  outline more 'jagged'
#' @param quantiles draws lines at the specified quantiles, eg c(0.25,0.5,0.25).
#'  If NULL, nothing is drawn
#' @param jitter if NA, then the data points will not be shown (only outliers!),
#'  otherwise it adds a random value to the x-values of the data points plotted.
#'  Note: If set to 0 then they will be located on a straight line
#' @param alpha alpha ('see through' value) of the data points
#' @param size size of the data points
#' @param shape shape of the datapoints (default = 16), see vignette
#'  ggplot2::ggplot2-specs
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param yLabel set y-axis title
#' @param title sets title of graph
#' @param vertical if TRUE, flips x- and y-axis
#' @param xDefault default is set to FALSE, together with xLimits this can be
#'  used to tweak the positioning and with of the box
#' @param xLimits default = c(-0.75,0.75), together with xDefault this can be
#'  used to tweak the positioning and with of the box
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#' 
#' @returns a ggplot object
#' @export
statViolinPlotSingle <- function(data, column = 1, removeNA = TRUE,
                                 sampleSize = NA,
                                 outlineColor = "black", fillColor = "red",
                                 outlineSize = 0.5, outlineType = "solid",
                                 violinAlpha = 1,
                                 scale = c("area","count","width")[1],
                                 trim = TRUE,
                                 bandwidth = 1, quantiles = NULL,
                                 jitter = NA, alpha = 0.5, size = 3, shape = 16,
                                 xAxis = FALSE, yAxis = TRUE,
                                 yLabel = ifelse(!is.Class(data,"data.frame"),
                                                 NA,
                                                 ifelse(is.character(column),
                                                        paste(column,
                                                              collapse = ""),
                                                        paste(
                                                          colnames(data)[column],
                                                          collapse = ", "))),
                                 title = NA,
                                 vertical = FALSE,
                                 xDefault = FALSE, xLimits = c(-0.75,0.75),
                                 yDefault = TRUE, yLimits = c(0,NA),
                                 ...){
  # in not data.frame, then make into one
  if (!is.Class(data, "data.frame")){
    data <- data.frame(y = data)
    if (is.character(column)){
      colnames(data) <- column
    } else {
      if (!identical(yLabel,NA)){
        colnames(data) <- yLabel
      } # otherwise leave as is
    }
    whichColumn <- colnames(data)
  } else {
    if (is.character(column)){
      whichColumn = column
    } else {
      whichColumn = colnames(data)[column]
    }
  }
  if (!is.na(sampleSize)){
    data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),]
  }
  g <- ggplot2::ggplot(data = data, ggplot2::aes(x = 0, y = !!dplyr::sym(whichColumn)))
  g <- g + ggplot2::geom_violin(na.rm = removeNA,
                       fill = fillColor, col = outlineColor,
                       size = outlineSize, linetype = outlineType,
                       alpha = violinAlpha, scale = scale, trim = trim,
                       adjust = bandwidth, draw_quantiles = quantiles)
  # g <- g + stat_boxplot(geom = "errorbar", width = whiskerWidth,
  #                       na.rm = removeNA)
  if (!identical(jitter,NA)){
    g <- g + ggplot2::geom_jitter(ggplot2::aes(x = 0, y = !!dplyr::sym(whichColumn)), na.rm = removeNA,
                         position = ggplot2::position_jitter(jitter),
                         fill = fillColor, col = outlineColor, alpha = alpha,
                         size = size, shape = shape)
  }
  g <- graphsAdjust(list(g), vertical = vertical,
                    xDefault = xDefault, xLimits = xLimits,
                    yDefault = yDefault, yLimits = yLimits,
                    ...)[[1]]
  g <- g + theme_minimal_adapted(xAxis = xAxis, yAxis = yAxis)
  return(g)
}

#' creates a ggplot object showing a violin plot of multiple columns
#' 
#' @param data the data to be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which columns are to be used for the boxplot.
#'  Can be integer or character (column name), note that if both (character)
#'  column and yLabel are defined, column is used as label for the Y-axis. If
#'  not defined, then all columns of the data.frame will be used.
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this will remove warning messages and errors
#' @param variableName sets the 'combined' name of the columns (IF there is more
#'  than one!)
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param outlineColor defines the color of the line around the 'violin'
#' @param fillColor defines the color of the boxes themselves. @Note: if the 
#'  number of colors does not match the number of columns then ggplot2 default
#'  colors will be used
#' @param outlineSize defines the width of the line around the 'violin'
#' @param outlineType defines the linetype of the line around the 'violin'
#' @param violinAlpha defines the alpha ('see through' value) the 'violin'
#' @param scale default = "area", other valid values are "count" and "width".
#'  if "area" (default), all violins have the same area (before trimming the
#'  tails). If "count", areas are scaled proportionally to the number of
#'   observations. If "width", all violins have the same maximum width.
#'   @note taken from ?geom_violin, there is no sense in setting this argument
#'   for a single violin plot
#' @param trim If TRUE (default), trim the tails of the violins to the range of
#'  the data. If FALSE, don't trim the tails. @note taken from ?geom_violin
#' @param bandwidth defines the 'adjust' parameter, which is the adjustment of
#'  smoothing bandwidth. 1/2 means half of default bandwidth --> makes the
#'  outline more 'jagged'
#' @param quantiles draws lines at the specified quantiles, eg c(0.25,0.5,0.25).
#'  If NULL, nothing is drawn
#' @param jitter if NA, then the data points will not be shown (only outliers!),
#'  otherwise it adds a random value to the x-values of the data points plotted.
#'  Note: If set to 0 then they will be located on a straight line
#' @param alpha alpha ('see through' value) of the data points
#' @param size size of the data points
#' @param shape shape of the datapoints (default = 16), see vignette
#'  ggplot2::ggplot2-specs
#' @param jitterFill fill color of the data points
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param xLabel set x-axis title
#' @param yLabel set y-axis title
#' @param title sets title of graph
#' @param showLegend defines if the legend is to be shown or not
#' @param legend.position defines where a legend is to be placed
#' @param vertical if TRUE, flips x- and y-axis
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#' 
#' @returns a ggplot object
#' @export
statViolinPlotMultiple <- function(data, column = 1:ncol(data),
                                   sampleSize = NA, removeNA = TRUE,
                                   variableName = "variable",
                                   outlineColor = "black", fillColor = NA,
                                   outlineSize = 0.5, outlineType = "solid",
                                   violinAlpha = 1,
                                   scale = c("area","count","width")[1],
                                   trim = TRUE,
                                   bandwidth = 1, quantiles = NULL,
                                   jitter = 0.05, alpha = 0.5, size = 3,
                                   shape = 16, jitterFill = "black",
                                   vertical = FALSE,
                                   xAxis = TRUE, yAxis = TRUE,
                                   yDefault = TRUE, yLimits = c(0,NA),
                                   xLabel = "", yLabel = "",
                                   title ="",
                                   showLegend = TRUE, legend.position = "bottom",
                                   ...){
  if (length(column) <= 1){
    return(NA)
  }
  if (!is.na(sampleSize)){
    data <- data[sample(1:nrow(data), sampleSize, replace = FALSE), column]
  } else {
    data <- data[,column]
  }
  data <- as.data.frame(reshape2::melt(data.table::as.data.table(data),
                             measure = 1:length(column),
                             variable.name = variableName,
                             value.name = "value"))
  if (removeNA){
    data <- data %>% stats::na.omit()
  }
  g <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = variableName, y = "value"))
  g <- g + ggplot2::geom_violin(na.rm = removeNA, col = outlineColor, 
                       ggplot2::aes_string(group = variableName,
                                  fill = variableName),
                       size = outlineSize, linetype = outlineType,
                       alpha = violinAlpha, scale = scale, trim = trim,
                       adjust = bandwidth, draw_quantiles = quantiles)
  if (!is.na(jitter)){
    g <- g + ggplot2::geom_jitter(ggplot2::aes_string(x = variableName, y = "value"),
                         position = ggplot2::position_jitter(jitter),
                         fill = jitterFill, col = outlineColor,
                         alpha = alpha, size = size, shape = shape)
  }
  if (length(fillColor) == length(column)){
    g <- g + ggplot2::scale_fill_manual(values = fillColor)
  }
  g <- graphsAdjust(list(g), vertical = vertical,
                    xDefault = TRUE,
                    yDefault = yDefault, yLimits = yLimits,
                    xLabel = xLabel, yLabel = yLabel, titles = title,
                    setTheme = theme_minimal_adapted(xAxis = xAxis,
                                                     yAxis = yAxis,
                                                     showLegend = showLegend,
                                                     legend.position = legend.position),
                    ...)[[1]]
  return(g)
}

#' creates a ggplot object showing a boxplot
#' 
#' @param data the data to be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which column is to be used for the boxplot.
#'  Can be integer or character (column name), note that if both (character)
#'  column and yLabel are defined, column is used as label for the Y-axis
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this will remove warning messages and errors 
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param outlineColor defines the color of the line around the box
#' @param fillColor defines the color of the box itself
#' @param jitter if NA, then the data points will not be shown (only outliers!),
#'  otherwise it adds a random value to the x-values of the data points plotted.
#'  Note: If set to 0 then they will be located on a straight line
#' @param alpha alpha ('see through' value) of the data points
#' @param size size of the data points
#' @param shape shape of the datapoints (default = 16), see vignette
#'  ggplot2::ggplot2-specs
#' @param whiskerWidth defines the width of the whiskers (0-1)
#' @param boxWidth defines the width of the box (0-1)
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param yLabel set y-axis title
#' @param title sets title of graph
#' @param xDefault default is set to FALSE, together with xLimits this can be
#'  used to tweak the positioning and with of the box
#' @param xLimits default = c(-0.75,0.75), together with xDefault this can be
#'  used to tweak the positioning and with of the box
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param vertical if TRUE, flips x- and y-axis
#' @param showMean defines if the mean value of the data should be shown
#' @param meanShape shape of the mean symbol (default = 23)
#' @param meanColor color of the line around the mean symbol
#' @param meanFill fill color of the shape of the mean symbol
#' @param meanSize size of the mean symbol
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#'  
#' @note box itself:
#'  bottom = 25% quantile, top = 75% quantile, middle = 50% quantile (median),
#'  lower whisker = 25% quantile + 1.5*IQR,
#'  upper whisker= 75% quantile + 1.5*IQR 
#'  IQR = (75% quantile) - (25% quantile) (Inter Quantile Range)
#' 
#' @returns a ggplot object
#' @export
statBoxPlotSingle <- function(data, column = 1, removeNA = TRUE,
                              sampleSize = NA,
                              outlineColor = "black", fillColor = "red",
                              jitter = 0.05, alpha = 0.5, size = 3, shape = 16,
                              whiskerWidth = 0.5, boxWidth = 0.5,
                              xAxis = FALSE,
                              yAxis = TRUE,
                              yLabel = ifelse(!is.Class(data,"data.frame"),
                                              NA,
                                              ifelse(is.character(column),
                                                     paste(column, collapse = ""),
                                                     paste(colnames(data)[column],
                                                           collapse = ", "))),
                              title = NA,
                              vertical = FALSE,
                              xDefault = FALSE, xLimits = c(-0.75,0.75),
                              yDefault = TRUE, yLimits = c(0,NA),
                              showMean = TRUE, meanShape = 23,
                              meanColor = "black", meanFill = "orange",
                              meanSize = 5,
                              ...){
  # in not data.frame, then make into one
  if (!is.Class(data, "data.frame")){
    data <- data.frame(y = data)
    if (is.character(column)){
      colnames(data) <- column
    } else {
      if (!identical(yLabel,NA)){
        colnames(data) <- yLabel
      } # otherwise leave as is
    }
    whichColumn <- colnames(data)
  } else {
    if (is.character(column)){
      whichColumn = column
    } else {
      whichColumn = colnames(data)[column]
    }
  }
  if (!is.na(sampleSize)){
    data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),]
  }
  g <- ggplot2::ggplot(data = data, ggplot2::aes(y = !!dplyr::sym(whichColumn)))
  if (!is.na(jitter)){
    g <- g + ggplot2::geom_boxplot(na.rm = removeNA, outlier.shape = NA,
                          fill = fillColor, col = outlineColor,
                          width = boxWidth)
    g <- g + ggplot2::stat_boxplot(geom = "errorbar", width = whiskerWidth,
                          na.rm = removeNA)
    g <- g + ggplot2::geom_jitter(ggplot2::aes(x = 0, y = !!dplyr::sym(whichColumn)), na.rm = removeNA,
                         position = ggplot2::position_jitter(jitter), 
                         fill = fillColor, col = outlineColor, alpha = alpha,
                         size = size, shape = shape)
  } else {
    g <- g + ggplot2::geom_boxplot(na.rm = removeNA, fill = fillColor,
                          col = outlineColor, width = boxWidth,
                          outlier.color = outlineColor,
                          outlier.fill = fillColor, outlier.alpha = alpha,
                          outlier.shape = shape, outlier.size = size)
    g <- g + ggplot2::stat_boxplot(geom = "errorbar", width = whiskerWidth,
                          na.rm = removeNA)
  }
  if (showMean){
    theMean <- mean(as.data.frame(data)[,whichColumn], na.rm = removeNA)
    g <- g + ggplot2::geom_point(ggplot2::aes(x = 0, y = theMean) ,shape = meanShape,
                        size = meanSize, col = meanColor, fill = meanFill)
  }
  g <- graphsAdjust(list(g), vertical = vertical,
                    xDefault = xDefault, xLimits = xLimits,
                    yDefault = yDefault, yLimits = yLimits,
                    ...)[[1]]
  g <- g + theme_minimal_adapted(xAxis = xAxis, yAxis = yAxis, ...)
  return(g)
}


#' creates a ggplot object showing a boxplot of multiple columns
#' 
#' @param data the data to be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which columns are to be used for the boxplot.
#'  Can be integer or character (column name), note that if both (character)
#'  column and yLabel are defined, column is used as label for the Y-axis. If
#'  not defined, then all columns of the data.frame will be used.
#' @param melted boolean that defines whether the specified columns still need
#'  to be melted into a single column for the graph. If melted = TRUE then
#'  the argument "column" should be a single column!
#' @param varColumn this boolean argument is only used in case melted = TRUE.
#'  It specifies the column to be used as variable name column
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this will remove warning messages and errors
#' @param variableName sets the 'combined' name of the columns, 
#'  must be a single word
#' @param outlineColor defines the color of the line around the box
#' @param fillColor defines the color of the boxes themselves. @Note: if the 
#'  number of colors does not match the number of columns then ggplot2 default
#'  colors will be used
#' @param jitter if NA, then the data points will not be shown (only outliers!),
#'  otherwise it adds a random value to the x-values of the data points plotted.
#'  Note: If set to 0 then they will be located on a straight line
#' @param alpha alpha ('see through' value) of the data (jitter) points
#' @param size size of the data (jitter) points
#' @param shape shape of the data (default = 16), see vignette
#'  ggplot2::ggplot2-specs
#' @param jitterFill defines color of the jitter (single color!)
#' @param whiskerWidth defines the width of the whiskers (0-1)
#' @param boxWidth defines the width of the box (0-1)
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param xLabel set x-axis title
#' @param yLabel set y-axis title
#' @param xDiscrete defines if x-axis should be/is discrete
#' @param title sets title of graph
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param vertical if TRUE, flips x- and y-axis
#' @param showMean defines if the mean value of the data should be shown
#' @param meanShape shape of the mean symbol (default = 23)
#' @param meanColor color of the line around the mean symbol
#' @param meanFill fill color of the shape of the mean symbol
#' @param meanSize size of the mean symbol
#' @param showLegend defines if the legend is to be shown or not
#' @param legend.title if not NA, then to give a non-default name to the legend
#' @param legend.position defines where a legend is to be placed
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#'  
#' @note box itself:
#'  bottom = 25% quantile, top = 75% quantile, middle = 50% quantile (median),
#'  lower whisker = 25% quantile + 1.5*IQR,
#'  upper whisker= 75% quantile + 1.5*IQR 
#'  IQR = (75% quantile) - (25% quantile) (Inter Quantile Range)
#' 
#' @returns a ggplot object
#' @export
statBoxPlotMultiple <- function(data, column = 1:ncol(data),
                                melted = FALSE, varColumn = NA,
                                sampleSize = NA, removeNA = TRUE,
                                variableName = "variable",
                                outlineColor = "black", fillColor = NA,
                                jitter = 0.05, alpha = 0.5, size = 3,
                                shape = 16, jitterFill = "black",
                                whiskerWidth = 0.5, boxWidth = 0.5,
                                vertical = FALSE,
                                xAxis = TRUE, yAxis = TRUE,
                                yDefault = TRUE, yLimits = c(0,NA),
                                xLabel = "", yLabel = "",
                                xDiscrete = TRUE,
                                title ="",
                                showMean = TRUE, meanShape = 23,
                                meanColor = "black", meanFill = "orange",
                                meanSize = 5,
                                legend.title = NA,
                                showLegend = TRUE, legend.position = "bottom",
                                ...){
  if ((length(column) <= 1) & !((length(column) == 1) & melted & !is.na(varColumn))){
    return(NA)
  }
  if (!is.character(column)){
    column = colnames(data)[column]
  }
  if (melted){
    if (!is.character(varColumn)){
      varColumn = colnames(data)[varColumn]
    }
  }
  if (!melted){
    if (!is.na(sampleSize)){
      data <- data[sample(1:nrow(data), sampleSize, replace = FALSE), column]
    } else {
      data <- data[,column]
    }
    data <- as.data.frame(reshape2::melt(data.table::as.data.table(data),
                               measure = 1:length(column),
                               variable.name = variableName,
                               value.name = "value"))
  } else {
    data <- data %>% dplyr::select(dplyr::all_of(varColumn), dplyr::all_of(column))
    colnames(data) <- c(variableName,"value")
  }
  if (removeNA){
    data <- data %>% stats::na.omit()
  }
  g <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = variableName, y = "value"))
  if (!is.na(jitter)){
    g <- g + ggplot2::geom_boxplot(na.rm = removeNA, col = outlineColor, 
                          outlier.shape = NA,
                          width = boxWidth, ggplot2::aes_string(group = variableName,
                                                       fill = variableName))
    g <- g + ggplot2::stat_boxplot(geom = "errorbar", width = whiskerWidth, ggplot2::aes_string(group = variableName))
    g <- g + ggplot2::geom_jitter(ggplot2::aes_string(x = variableName, y = "value"),
                         position = ggplot2::position_jitter(jitter),
                         fill = jitterFill, col = outlineColor, alpha = alpha, size = size, shape = shape)
  } else {
    g <- g + ggplot2::geom_boxplot(na.rm = removeNA, col = outlineColor, 
                          width = boxWidth, ggplot2::aes_string(group = variableName,
                                                       fill = variableName),
                          outlier.color = outlineColor, outlier.fill = jitterFill, outlier.alpha = alpha,
                          outlier.shape = shape, outlier.size = size)
    g <- g + ggplot2::stat_boxplot(geom = "errorbar", width = whiskerWidth, ggplot2::aes_string(group = variableName))
  }
  if (showMean){
    value = NULL  # for work around purposes only
    means <- data %>%
      dplyr::group_by(!!dplyr::sym(variableName)) %>%
      dplyr::summarize(theMean = mean(value, na.rm = removeNA))
    g <- g + ggplot2::geom_jitter(data = means, ggplot2::aes_string(x = variableName, y = "theMean"),
                         shape = meanShape, size = meanSize, col = meanColor, fill = meanFill,
                         width = 0, height = 0)
  }
  if (!identical(fillColor,NA)){
    if (!melted){
      if (length(fillColor) == length(column)){
        g <- g + ggplot2::scale_fill_manual(values = fillColor)
      }
    } else {
      if (length(fillColor) == length(levels(factor(data$variable)))){
        g <- g + ggplot2::scale_fill_manual(values = fillColor)
      }
    }
  }
  g <- graphsAdjust(list(g), vertical = vertical,
                    xDefault = TRUE,
                    yDefault = yDefault, yLimits = yLimits,
                    xDiscrete = xDiscrete,
                    xLabel = xLabel, yLabel = yLabel, titles = title,
                    setTheme = theme_minimal_adapted(xAxis = xAxis,
                                                     yAxis = yAxis,
                                                     showLegend = showLegend,
                                                     legend.position = legend.position),
                    ...)[[1]]
  if (identical(legend.title,NA)){
    return(g)
  } else {
    return(g + ggplot2::scale_fill_discrete(name = legend.title))
  }
}


#' creates a ggplot object showing a boxplot of a single column, split (cut)
#'  along another column (or itself)
#'  
#' @param data the data to be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which column are to be used for the boxplot.
#'  Can be integer or character (column name), note that if both (character)
#'  column and yLabel are defined, column is used as label for the Y-axis. If
#'  not defined, then all columns of the data.frame will be used.
#' @param varColumn defines which column is to be used to split the data column
#'  (argument: column). Can be integer or character (column name). The splitting
#'  is performed via the function cut(). See ?base::cut for details. Note that
#'  if the varColumn contains non-numeric data (eg character or factor), no
#'  split will be performed
#' @param varBreaks specfies how to split the varColumn, see ?base::cut (breaks
#'  argument). Note that varBreaks and other arguments specifying the split are
#'  ignored if the varColumn is not numerical
#' @param varLabels specfies labels to use when splitting the varColumn,
#'  see ?base::cut (labels argument).
#' @param forceLabels with this boolean the labels specified by the argument
#'  varLabels can be enforced onto the x-axis if needed
#' @param varIncludeLowest specfies the include.lowest argument of base::cut,
#'  see ?base::cut
#' @param varRight specfies the right argument of base::cut, see ?base::cut
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this will remove warning messages and errors
#' @param variableName sets the 'combined' name of the columns, 
#'  must be a single word
#' @param outlineColor defines the color of the line around the box
#' @param fillColor defines the color of the boxes themselves. @Note: if the 
#'  number of colors does not match the number of columns then ggplot2 default
#'  colors will be used
#' @param jitter if NA, then the data points will not be shown (only outliers!),
#'  otherwise it adds a random value to the x-values of the data points plotted.
#'  Note: If set to 0 then they will be located on a straight line
#' @param alpha alpha ('see through' value) of the data (jitter) points
#' @param size size of the data (jitter) points
#' @param shape shape of the data (default = 16), see vignette
#'  ggplot2::ggplot2-specs
#' @param jitterFill defines color of the jitter (single color!)
#' @param whiskerWidth defines the width of the whiskers (0-1)
#' @param boxWidth defines the width of the box (0-1)
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param xLabel set x-axis title
#' @param yLabel set y-axis title
#' @param xDiscrete defines if x-axis should be/is discrete
#' @param title sets title of graph
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param vertical if TRUE, flips x- and y-axis
#' @param showMean defines if the mean value of the data should be shown
#' @param meanShape shape of the mean symbol (default = 23)
#' @param meanColor color of the line around the mean symbol
#' @param meanFill fill color of the shape of the mean symbol
#' @param meanSize size of the mean symbol
#' @param showLegend defines if the legend is to be shown or not
#' @param legend.position defines where a legend is to be placed
#' @param returnData if TRUE then a list with 2 elements is returned. The first
#'  element is the data.frame used to generate the graph and the second element
#'  is the graph itself
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#'  
#' @note box itself:
#'  bottom = 25% quantile, top = 75% quantile, middle = 50% quantile (median),
#'  lower whisker = 25% quantile + 1.5*IQR,
#'  upper whisker= 75% quantile + 1.5*IQR 
#'  IQR = (75% quantile) - (25% quantile) (Inter Quantile Range)
#' 
#' @returns a ggplot object or a list
#' @export
statBoxPlotMultipleVar <- function(data, column = 1,
                                   varColumn = 2, varBreaks = 4,
                                   varLabels = NA, forceLabels = FALSE,
                                   varIncludeLowest = FALSE, varRight = TRUE, 
                                   sampleSize = NA, removeNA = TRUE,
                                   variableName = "variable",
                                   outlineColor = "black", fillColor = NA,
                                   jitter = 0.05, alpha = 0.5, size = 3,
                                   shape = 16, jitterFill = "black",
                                   whiskerWidth = 0.5, boxWidth = 0.5,
                                   vertical = FALSE,
                                   xAxis = TRUE, yAxis = TRUE,
                                   yDefault = TRUE, yLimits = c(0,NA),
                                   xLabel = "", yLabel = "",
                                   xDiscrete = TRUE,
                                   title ="",
                                   showMean = TRUE, meanShape = 23,
                                   meanColor = "black", meanFill = "orange",
                                   meanSize = 5,
                                   showLegend = TRUE, legend.position = "bottom",
                                   returnData = FALSE,
                                   ...){
  if (length(column) > 1){
    return(NA)
  }
  if (is.character(column)){
    whichColumn = column
  } else {
    whichColumn = colnames(data)[column]
  }
  if (!is.na(sampleSize)){
    data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),]
  }
  if (is.character(varColumn)){
    whichVarColumn = varColumn
  } else {
    whichVarColumn = colnames(data)[varColumn]
  }
  if (!identical(varLabels,NA)){
    if (length(varBreaks) == 1){
      if (varBreaks != length(varLabels)){
        return(NA)
      }
    } else {
      if ((length(varBreaks)-1) != length(varLabels)){
        return(NA)
      }
    }
  } else {
    varLabels <- NULL
  }
  isVarNumeric <- (is.Class(data[whichVarColumn][,1],"numeric") |
                     is.Class(data[whichVarColumn][,1],"integer"))
  if (isVarNumeric){
    data$cut <- cut(data[whichVarColumn][,1],
                    breaks = varBreaks,
                    labels = varLabels,
                    include.lowest = varIncludeLowest,
                    right = varRight)
  }
  if (!is.na(sampleSize)){
    if (isVarNumeric){
      data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),] %>%
        dplyr::select(dplyr::all_of(whichColumn), dplyr::all_of(varColumn),dplyr::all_of("cut"))
    } else {
      data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),] %>%
        dplyr::select(dplyr::all_of(whichColumn), dplyr::all_of(varColumn))
    }
  } else {
    if (isVarNumeric){
      data <- data %>% dplyr::select(dplyr::all_of(whichColumn), dplyr::all_of(varColumn),dplyr::all_of("cut"))
    } else {
      data <- data %>% dplyr::select(dplyr::all_of(whichColumn), dplyr::all_of(varColumn))
    }
  }
  if (removeNA){
    data <- data %>% stats::na.omit()
  }
  g <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = ifelse(isVarNumeric,
                                                 "cut",
                                                 varColumn),
                                      y = whichColumn))
  if (identical(fillColor,NA)){
    if (isVarNumeric){
      g <- g + ggplot2::geom_boxplot(na.rm = removeNA, outlier.shape = NA,
                            col = outlineColor,
                            ggplot2::aes(fill = cut),
                            width = boxWidth)
    } else {
      g <- g + ggplot2::geom_boxplot(na.rm = removeNA, outlier.shape = NA,
                            col = outlineColor,
                            ggplot2::aes_string(fill = varColumn),
                            width = boxWidth)
    }
  } else {
    if (isVarNumeric){
      g <- g + ggplot2::geom_boxplot(na.rm = removeNA, outlier.shape = NA,
                            col = outlineColor,
                            ggplot2::aes(fill = cut),
                            width = boxWidth)
    } else {
      g <- g + ggplot2::geom_boxplot(na.rm = removeNA, outlier.shape = NA,
                            col = outlineColor,
                            ggplot2::aes_string(fill = varColumn),
                            width = boxWidth)
    }
  }
  g <- g + ggplot2::stat_boxplot(geom = "errorbar", width = whiskerWidth,
                        na.rm = removeNA)
  if (!is.na(jitter)){
    g <- g + ggplot2::geom_jitter(ggplot2::aes_string(x = ifelse(isVarNumeric,
                                               "cut",
                                               varColumn),
                                    y = whichColumn), na.rm = removeNA,
                         position = ggplot2::position_jitter(jitter), 
                         fill = jitterFill, col = outlineColor, alpha = alpha,
                         size = size, shape = shape)
  }
  if (showMean){
    means <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(ifelse(isVarNumeric,
                                    "cut",
                                    varColumn)))) %>%
      dplyr::summarize(theMean = mean(!!dplyr::sym(whichColumn), na.rm = removeNA))
    g <- g + ggplot2::geom_jitter(data = means, ggplot2::aes_string(x = ifelse(isVarNumeric,
                                                             "cut",
                                                             varColumn),
                                                  y = "theMean"),
                         shape = meanShape, size = meanSize,
                         col = meanColor, fill = meanFill,
                         width = 0, height = 0)
  }
  if (!identical(fillColor,NA)){
    if (length(varBreaks) == 1){
      if (length(fillColor) == varBreaks){
        g <- g + ggplot2::scale_fill_manual(values = fillColor)
      }
    } else {
      g <- g + ggplot2::scale_fill_manual(values = fillColor)
    }
  }
  g <- graphsAdjust(list(g), vertical = vertical,
                    xDefault = TRUE,
                    yDefault = yDefault, yLimits = yLimits,
                    xDiscrete = xDiscrete,
                    xLabel = xLabel, yLabel = yLabel, titles = title,
                    setTheme = theme_minimal_adapted(xAxis = xAxis,
                                                     yAxis = yAxis,
                                                     showLegend = showLegend,
                                                     legend.position = legend.position, ...),
                    ...)[[1]]
  if (!identical(varLabels, NA)){
    if (forceLabels){
      g <- g + ggplot2::scale_x_discrete(limits = varLabels)
    }
  }
  if (returnData){
    tempList <- list()
    tempList[[1]] <- data
    tempList[[2]] <- g
    return(tempList)
  } else {
    return(g)
  }
}

#' takes a lists of data.frame's and generates a boxplot using data from the
#'  different data.frame's in the list as elements in the boxplot
#'  
#' @param data the data to be plotted. Must be a list of data.frame's with each
#'  at least the same idColumn & varColumn present
#' @param idColumn defines which column is to be used for x-axis of the boxplot.
#'  Can be integer or character (column name)
#' @param varColumn defines which column is to be used for the y-axis of the
#'  boxplot
#' @param sortIDs if TRUE then data in the idColumn will be sorted
#' @param sortDescending if TRUE then the data will be sorted in a descending
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this will remove warning messages and errors
#' @param variableName sets the 'combined' name of the columns, 
#'  must be a single word
#' @param valueName sets the name of the 'y'-values, must be a single word
#' @param outlineColor defines the color of the line around the box
#' @param fillColor defines the color of the boxes themselves. Note: if the 
#'  number of colors does not match the number of columns then ggplot2 default
#'  colors will be used
#' @param jitter if NA, then the data points will not be shown (only outliers!),
#'  otherwise it adds a random value to the x-values of the data points plotted.
#'  Note: If set to 0 then they will be located on a straight line
#' @param alpha alpha ('see through' value) of the data (jitter) points
#' @param size size of the data (jitter) points
#' @param shape shape of the data (default = 16), see vignette
#'  ggplot2::ggplot2-specs
#' @param jitterFill defines color of the jitter (single color!)
#' @param whiskerWidth defines the width of the whiskers (0-1)
#' @param boxWidth defines the width of the box (0-1)
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param xLabel set x-axis title
#' @param yLabel set y-axis title
#' @param xDiscrete defines if x-axis should be/is discrete
#' @param title sets title of graph
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param vertical if TRUE, flips x- and y-axis
#' @param showMean defines if the mean value of the data should be shown
#' @param meanShape shape of the mean symbol (default = 23)
#' @param meanColor color of the line around the mean symbol
#' @param meanFill fill color of the shape of the mean symbol
#' @param meanSize size of the mean symbol
#' @param returnData if TRUE then a list with 2 elements is returned. The first
#'  element is the data.frame used to generate the graph and the second element
#'  is the graph itself
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#'
#' @note the data in the idColumns & varColumns are essentially x,y paurs, this
#'  function combines all the x's from the different data.frame's in the list
#'  and then makes a boxplot. Example code:
#'  l <- list()
#'  l[[1]] <- data.frame(name = c("P1","P2","P3","P4"), value = c(1,2,3,2))
#'  l[[2]] <- data.frame(name = c("P1","P2","P3","P4"), value = c(4,5,10,5.5))
#'  l[[3]] <- data.frame(name = c("P1","P2","P3","P4"), value = c(10,2,23,5))
#'  l[[4]] <- data.frame(name = c("P1","P2","P4"), value = c(10,2,6.7))
#'  statBoxPlotMultiTable(l, idColumn = "name",
#'                         varColumn = "value", returnData = T)
#'
#' @note box itself:
#'  bottom = 25% quantile, top = 75% quantile, middle = 50% quantile (median),
#'  lower whisker = 25% quantile + 1.5*IQR,
#'  upper whisker= 75% quantile + 1.5*IQR 
#'  IQR = (75% quantile) - (25% quantile) (Inter Quantile Range)
#' 
#' @returns a ggplot object or a list
#' @export
statBoxPlotMultiTable <- function(data, idColumn = 1, varColumn = 2,  
                                  sortIDs = FALSE, sortDescending = FALSE,
                                  sampleSize = NA, removeNA = FALSE,
                                  variableName = "variable",
                                  valueName = "value",
                                  outlineColor = "black", fillColor = "red",
                                  jitter = 0.05, alpha = 0.5, size = 3,
                                  shape = 16, jitterFill = "black",
                                  whiskerWidth = 0.5, boxWidth = 0.5,
                                  vertical = FALSE,
                                  xAxis = TRUE, yAxis = TRUE,
                                  xDiscrete = TRUE,
                                  yDefault = TRUE, yLimits = c(0,NA),
                                  xLabel = "", yLabel = "",
                                  title ="",
                                  showMean = TRUE, meanShape = 23,
                                  meanColor = "black", meanFill = "orange",
                                  meanSize = 5,
                                  returnData = FALSE,
                                  ...){
  if (!is.character(idColumn)){
    idColumn = colnames(data[[1]])[idColumn]
  }
  if (!is.na(sampleSize)){
    data <- lapply(data, function(x){
      x <- x[sample(1:nrow(data[x]), sampleSize, replace = FALSE),]
    })
  }
  data <- lapply(data, function(x){
    dplyr::bind_cols(x %>% dplyr::select(dplyr::all_of(idColumn)),
              x %>% dplyr::select(-dplyr::all_of(idColumn)) %>%    # cannot export where() normally from tidyselect 
                dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.character),~as.numeric(.x))))
  })
  if (!is.character(varColumn)){
    varColumn = colnames(data[[1]])[varColumn]
  }
  data <- lapply(data, function(x){
    x <- x[,c(idColumn, varColumn)]
  })
  if (sortIDs){
    if (sortDescending){
      data <- lapply(data, function(x){
        x <- x %>% dplyr::arrange(dplyr::desc(dplyr::across(dplyr::all_of(idColumn))))
      })
    } else {
      data <- lapply(data, function(x){
        x <- x %>% dplyr::arrange(dplyr::across(dplyr::all_of(idColumn)))
      })
    }
    xTickLabels <- as.character(data[[1]][,1])
  } else {
    xTickLabels <- NA
  }
  suppressMessages(
  data <- dplyr::bind_rows(lapply(lapply(data,t), function(x){
    x <- as.data.frame(x)
    colnames(x) <- as.character(x[1,])
    row.names(x) <- NULL
    x <- x[-1,]
    return(x)
  })))
  data <- data %>% dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.character),~as.numeric(.x)))
  g <- statBoxPlotMultiple(data = data, column = 1:ncol(data),
                           sampleSize = NA, removeNA = removeNA,
                           variableName = variableName,
                           outlineColor = outlineColor,
                           fillColor = rep(fillColor,ncol(data)),
                           jitter = jitter, alpha = alpha, size = size,
                           shape = shape, jitterFill = jitterFill,
                           whiskerWidth = whiskerWidth,
                           boxWidth = boxWidth, vertical = vertical,
                           xAxis = xAxis, yAxis = yAxis,
                           xDiscrete = xDiscrete,
                           yDefault = yDefault, yLimits = yLimits,
                           xLabel = xLabel, yLabel = yLabel,
                           title = title,
                           showMean = showMean, meanShape = meanShape,
                           meanColor = meanColor, meanFill = meanFill,
                           meanSize = meanSize, showLegend = FALSE, ...)
  if (!identical(xTickLabels, NA)){
    g <- g + ggplot2::scale_x_discrete(labels=xTickLabels)
  }
  if (!returnData){
    return(g)
  } else {
    return(list(data, g))
  }
}

#' generates a simple barplot of data with id's in one column (x-axis values)
#'  and the y-axis values in the varColumn
#' 
#' @param data data.frame with 1 (numeric) column to be used as x-values and 1
#'  or more columns to be used as bars (y-axis)
#' @param idColumn defines which column in the data is to be used as id of the
#'  bars in the barplot
#' @param varColumn one or more columns with numeric data, defining the height
#'  of the bars in the barplot
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. Note: this will remove warning messages and errors
#' @param variableName allows setting of the 'name' of the varColumns (x-value
#'  o/t bars, eg concentration)
#' @param valueName allows for setting of the name of the y-axis (eg signal)
#' @param barPosition sets the dodging etc of the bars in the barplot, see
#'  ?ggplot2::position_dodge for more info
#' @param fillColors sets the colors to be used for the bars
#' @param fillPalette to set more subtle colors for the bars, examples: "Blues",
#'  "Greens", etc
#' @param outlineColor sets the outline color of the bars
#' @param outlineType sets the linetype for the outline of the bars
#' @param outlineWidth sets the width of the outline of the bars
#' @param fillAlpha sets the alpha (shine through) value of the fill colors of
#'  the bars
#' @param vertical if TRUE then plot will be rotated 90 degrees
#' @param yAxis if TRUE then the y-Axis will be shown
#' @param yDefault if TRUE then default y-axis is used (based on the values in
#'  the barplot), if FALSE then the yLimits parameter is used to set the y-Axis
#'  range
#' @param yLimits two value numeric vector, specifying the minimum and maximum
#'  value of the range of the y-Axis
#' @param ySpace specifies how much space there should be between the maximum
#'  y-value in the barplot and the top of the plot. Value of 0 is no space, a
#'  value of 0.1 is equivalent to extending the y-range 10 percent
#' @param xLabel specifies the label on the x-axis
#' @param yLabel specifies the label on the y-axis
#' @param title specifies the title of the barplot
#' @param gridLines if TRUE then gridlines are shown
#' @param showLegend if TRUE then the legend is shown
#' @param legend.position defines the place of the legend (default = "bottom")
#' @param returnData if TRUE then a list with 2 elements is returned. The first
#'  element is the data.frame used to generate the graph and the second element
#'  is the graph itself
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#' 
#' example code:
#' ll <- data.frame(id = c(1,2,3,4), 
#' P1 = c(1,2,3,2), 
#' P2 = c( 4.0, 5.0,10.0, 5.5), 
#' P3 = c(10, 2,23, 5), 
#' P4 = c(10.0, 2.0, 6.7, 3.0))
#' statBarPlot(ll, idColumn = "id", varColumn = 1:5, returnData = T)
#' 
#' @returns a ggplot object or a list
#' @export
statBarPlot <- function(data, idColumn = 1,
                        varColumn = 2:ncol(data),  
                        sampleSize = NA, removeNA = FALSE,
                        variableName = "Variable",
                        valueName = "Value",
                        barPosition = ggplot2::position_dodge(),
                        fillColors = NA, # colors > palette
                        fillPalette = NA, # eg "Blues"
                        outlineColor = 0, outlineType = "solid",
                        outlineWidth = 1,
                        fillAlpha = 1,
                        vertical = FALSE,
                        yAxis = TRUE,
                        yDefault = TRUE, yLimits = c(0,NA),
                        ySpace = 0.1,
                        xLabel = idColumn, yLabel = valueName,
                        title ="",
                        gridLines = TRUE,
                        showLegend = TRUE, legend.position = "bottom",
                        returnData = FALSE,
                        ...){
  if (!is.character(idColumn)){
    idColumn = colnames(data)[idColumn]
  }
  if (!is.na(sampleSize)){
    data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),]
  }
  if (!is.character(varColumn)){
    varColumn = colnames(data)[varColumn]
  }
  data = reshape2::melt(data,
                        id.vars = idColumn,
                        measure.vars = varColumn,
                        variable.name = variableName,
                        value.name = valueName)
  g <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = idColumn,
                                      y = valueName,
                                      fill = variableName))
  g <- g + ggplot2::geom_bar(position = barPosition, stat = "identity",
                    linetype = outlineType, color = outlineColor,
                    size = outlineWidth, alpha = fillAlpha)
  if (yDefault){
    yDefault <- FALSE
    minY <- min(data[,valueName])
    maxY <- max(data[,valueName])
    if (minY >= 0){
      yLimits <- c(0, (1+ySpace)*maxY)
    } else {
      yLimits <- c(0, (1+ySpace)*minY)
    }
  }
  g <- graphsAdjust(list(g), vertical = vertical,
                    xDefault = TRUE,
                    yDefault = yDefault, yLimits = yLimits,
                    xLabel = xLabel, yLabel = yLabel, titles = title,
                    setTheme = theme_minimal_adapted(xAxis = TRUE,
                                                     yAxis = yAxis,
                                                     showLegend = showLegend,
                                                     legend.position = legend.position,
                                                     gridLines = gridLines,
                                                     ...),
                    ...)[[1]]
  if (!identical(fillColors,NA)){
    g <- g + ggplot2::scale_fill_manual(values = fillColors)
  } else {
    if (!identical(fillPalette,NA)){
      g <- g + ggplot2::scale_fill_brewer(palette = fillPalette)
    }
  }
  if (!returnData){
    return(g)
  } else {
    return(list(data, g))
  }
}

#' creates a ggplot object showing a normal quantile plot
#' 
#' @param data the data to be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which columns are to be used for the plot.
#'  Can be integer or character (column name)
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. Note: this will remove warning messages and errors
#' @param pointColor defines the color of the border of the data points
#' @param pointFill defines the color of the data points themselves
#' @param pointAlpha alpha ('see through' value) of the data points
#' @param pointShape shape of the data points
#' @param pointSize size of the data points
#' @param lineColor color of the normal distribution 'line'
#' @param lineType type of the normal distribution 'line'
#' @param lineWidth width of the normal distribution 'line'
#' @param lineAlpha alpha ('see through' value) of the normal distribution
#'  'line'
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param title sets title of graph
#' @param xDefault default is set to TRUE, together with xLimits, this can be
#'  used to define the exact range of the X-axis
#' @param xLimits  default = c(0,NA), together with xDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param vertical if TRUE, flips x- and y-axis
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#' 
#' @returns a ggplot object
#' @export
normalQuantilePlot <- function(data, column, removeNA = TRUE, sampleSize = NA,
                               pointColor = "black", pointFill = "green",
                               pointShape = 21, pointSize = 3,
                               pointAlpha = 0.75,
                               lineColor = "red", lineType = "dotted",
                               lineWidth = 1, lineAlpha = 1,
                               title = NULL,
                               xAxis = TRUE, yAxis = TRUE, vertical = FALSE,
                               xDefault = FALSE, xLimits = c(0,NA),
                               yDefault = TRUE, yLimits = c(0,NA),
                               ...){
  if (is.Class(data,"data.frame")){
    data <- data[,column]
  }
  if (removeNA) {
    data <- data %>% stats::na.omit()
  }
  if (!is.na(sampleSize)){
    data <- data[sample(1:length(data), sampleSize, replace = FALSE)]
  }
  qpoints <- stats::ppoints(length(data))
  theoryQuantiles <- stats::qnorm(qpoints, mean = 0, sd = 1)
  theoryQuantiles <- ((theoryQuantiles - mean(theoryQuantiles))/((max(theoryQuantiles) - min(theoryQuantiles)))) + 0.5
  data <- data.frame(Sample = data %>% sort(), Theoretical = theoryQuantiles)
  g <- ggplot2::ggplot(data = data, ggplot2::aes_string("Theoretical","Sample"))
  g <- g + ggplot2::geom_point(col = pointColor, fill = pointFill, shape = pointShape, size = pointSize, alpha = pointAlpha)
  qlm <-stats::lm(data = data, Sample~Theoretical)
  g <- g + ggplot2::geom_abline(slope = stats::coef(qlm)[2], intercept = stats::coef(qlm)[1],
                       col = lineColor, linetype = lineType, size = lineWidth, alpha = lineAlpha)
  g <- graphsAdjust(list(g), vertical = vertical, titles = title,
                    xDefault = xDefault, xLimits = xLimits,
                    yDefault = yDefault, yLimits = yLimits,
                    ...)[[1]]
  g <- g + theme_minimal_adapted(xAxis = xAxis, yAxis = yAxis)
  return(g)
}

#' creates a ggplot object showing a normal quantile plot with
#'  confidence intervals
#' 
#' @param data the data to be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which columns are to be used for the plot.
#'  Can be integer or character (column name)
#' @param sampleSize allows to the use of a sample of the data to be used for
#'  the boxplot. By default sampleSize = NA, in which case all data is used
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this will remove warning messages and errors
#' @param pointColor defines the color of the border of the data points
#' @param pointFill defines the color of the data points themselves
#' @param pointAlpha alpha ('see through' value) of the data points
#' @param pointShape shape of the data points
#' @param pointSize size of the data points
#' @param lineColor color of the normal distribution 'line'
#' @param lineType type of the normal distribution 'line'
#' @param lineWidth width of the normal distribution 'line'
#' @param lineAlpha alpha ('see through' value) of the normal distribution
#'  'line'
#' @param bandType sets the type of confidence bands calculations.
#'  Default = "ks" (Kolmogorov-Smirnov test), for descriptions of the other
#'  options ("pointwise","boot","ts") see ?geom_qq_band
#' @param bandFill sets the color of the confidence interval band
#' @param bandAlpha sets the alpha ('see through' value) of the confidence
#'  interval band
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param title sets title of graph
#' @param xDefault default is set to TRUE, together with xLimits, this can be
#'  used to define the exact range of the X-axis
#' @param xLimits  default = c(0,NA), together with xDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param vertical if TRUE, flips x- and y-axis
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#' 
#' @returns a ggplot object
#' @export
normalQQPlot <- function(data, column, removeNA = TRUE,
                         sampleSize = NA,
                         pointColor = "black", pointFill = "green",
                         pointShape = 21, pointSize = 3, pointAlpha = 0.75,
                         lineColor = "red", lineType = "solid",
                         lineWidth = 1, lineAlpha = 1,
                         title = NULL,
                         bandType = c("pointwise","boot","ks","ts")[3], 
                         bandFill = "blue", bandAlpha = 0.10,
                         xAxis = TRUE, yAxis = TRUE, vertical = FALSE,
                         xDefault = TRUE, xLimits = c(0,NA),
                         yDefault = TRUE, yLimits = c(0,NA),
                         ...){
  if (is.Class(data,"data.frame")){
    data <- data[,column]
  }
  if (removeNA) {
    data <- data %>% stats::na.omit()
  }
  if (!is.na(sampleSize)){
    data <- data[sample(1:length(data), sampleSize, replace = FALSE)]
  }
  g <- ggplot2::ggplot(data = data.frame(x = data), ggplot2::aes_string(sample = "x"))
  g <- g + qqplotr::geom_qq_band(bandType = bandType, alpha = bandAlpha, fill = bandFill)
  g <- g + qqplotr::stat_qq_point(col = pointColor, fill = pointFill,
                         shape = pointShape, size = pointSize,
                         alpha = pointAlpha)
  g <- g + qqplotr::stat_qq_line(col = lineColor, linetype = lineType, size = lineWidth, alpha = lineAlpha)
  g <- graphsAdjust(list(g), vertical = vertical, titles = title,
                    xDefault = xDefault, xLimits = xLimits,
                    yDefault = yDefault, yLimits = yLimits,
                    ...)[[1]]
  
  g <- g + theme_minimal_adapted(xAxis = xAxis, yAxis = yAxis, showLegend = FALSE)
  return(g)
}

#' Creates a data.frame with the values provided to be used in the controlChart
#'  function as controlLines
#'  
#' @param yValues a numeric vector specifying the 'heights' or y-axis values
#'  where a horizontal line is needed
#' @param type linetype of the horizontal lines needed, either a single value
#'  or a vector of same length as yValues
#' @param color color of the horizontal lines needed, either a single value
#'  or a vector of same length as yValues
#' @param width width of the horizontal lines needed, either a single value
#'  or a vector of same length as yValues
#' @param alpha alpha ('see through' value) of the horizontal lines needed,
#'  either a single value or a vector of same length as yValues
#'   
#' @note if yValues == NA or empty then the function returns NA
#'   
#'  @returns a data.frame with columns: yValues, type, color, width, alpha
#'  @export
controlChartMarkerLines <- function(yValues = NA,
                                    type = "dashed",
                                    color = "blue",
                                    width = 1,
                                    alpha = 0.75){
  if (identical(yValues,NA) | purrr::is_empty(yValues)){
    return(NA)
  }
  return(data.frame(yValues = yValues,
                    type = type,
                    color = color,
                    width = width,
                    alpha = alpha))
}

#' creates a ggplot object showing a control chart with optional control lines
#' 
#' @param data the data to be plotted, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param yColumn defines which column is to be used for y-axis ib the plot.
#'  Can be integer or character (column name)
#' @param xColumn defines which column is to be used for the x-axis, can be
#'  integer or character (column name). If the column consists of date/time
#'  values, then set the parameter xDefault to TRUE. Note: this parameter is
#'  optional, default is NA
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this will remove warning messages and errors
#' @param drawPoints boolean, if TRUE then the data points themselves will be
#'  drawn
#' @param pointColor defines the color of the border of the data points
#' @param pointFill defines the color of the data points themselves
#' @param pointAlpha alpha ('see through' value) of the data points
#' @param pointShape shape of the data points
#' @param pointSize size of the data points
#' @param drawLine boolean, if TRUE then a line through the data points will be
#'  drawn
#' @param lineColor color of the line
#' @param lineType type of the line
#' @param lineWidth width of the line
#' @param lineAlpha alpha ('see through' value) of the line
#' @param controlLines either NA (no horizontal lines) or a data.frame with
#'  columns yValues, type, color, width, alpha. yValues defines at which
#'  'height' (y-axis) the control lines are to be drawn. Use the function
#'   controlChartMarkerLines for this if needed
#' @param xLabel defines x-axis label
#' @param yLabel defines y-axis label (if not defined (NA), then the yColumn
#'  name will be used, if possible)
#' @param xAxis defines if the x-axis is shown
#' @param yAxis defines if the x-axis is shown
#' @param title sets title of graph
#' @param xDefault default is set to TRUE, together with xLimits, this can be
#'  used to define the exact range of the X-axis
#' @param xLimits  default = c(0,NA), together with xDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param vertical if TRUE, flips x- and y-axis
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  (like xLimits, xExpand, etc)
#' 
#' @returns a ggplot object
#' @export
controlChart <- function(data, yColumn, xColumn = NA, removeNA = TRUE,
                         drawPoints = TRUE,
                         pointColor = "black", pointFill = "orange",
                         pointShape = 21, pointSize = 3, pointAlpha = 0.75,
                         drawLine = TRUE,
                         lineColor = "black", lineType = "solid",
                         lineWidth = 1, lineAlpha = 1,
                         controlLines = controlChartMarkerLines(yValues = NA),
                         xLabel = "#", yLabel = NA,
                         title = paste("Control Chart ",
                                       ifelse(identical(yLabel, NA),
                                              "",
                                              paste("of ",yLabel, sep = "")),
                                       sep = ""),
                         xAxis = TRUE, yAxis = TRUE, vertical = FALSE,
                         xDefault = FALSE, xLimits = c(NA,NA),
                         yDefault = TRUE, yLimits = c(NA,NA),
                         ...){
  if (is.Class(data,"data.frame")){
    if ((length(yColumn) > 1) | (length(xColumn) > 1)){
      return(NA)
    }
    coln <- colnames(data)
    if (identical(xColumn,NA)){
      data <- data.frame(x = 1:nrow(data), y = data[,yColumn])
    } else {
      data <- data.frame(x = data[, xColumn], y = data[,yColumn])
    }
    colnames(data) <- c(ifelse(identical(xColumn,NA),
                               "x",
                               ifelse(is.character(xColumn),
                                      xColumn,
                                      coln[xColumn])),
                        ifelse(is.character(yColumn),
                               yColumn,
                               coln[yColumn]))
  } else {
    data <- data.frame(x = 1:length(data),
                       y = data)
    colnames(data) <- c("x","y")
  }
  if (removeNA) {
    data <- data %>% stats::na.omit()
  }
  g <- ggplot2::ggplot(data = data, ggplot2::aes_string(colnames(data)[1],colnames(data)[2]))
  if (drawPoints){
    g <- g + ggplot2::geom_point(col = pointColor, fill = pointFill, shape = pointShape, size = pointSize, alpha = pointAlpha)
  }
  if (drawLine) {
    g <- g + ggplot2::geom_line(col = lineColor, linetype = lineType, size = lineWidth, alpha = lineAlpha)
  }
  if (!identical(controlLines,NA)){
    for (counter in 1:(nrow(controlLines))){
      g <- g + ggplot2::geom_hline(yintercept = controlLines$yValues[counter],
                          col = controlLines$color[counter],
                          size = controlLines$width[counter],
                          linetype = controlLines$type[counter],
                          alpha = controlLines$alpha[counter])
    }
  }
  g <- graphsAdjust(list(g), vertical = vertical,
                    xLabel = xLabel, yLabel = yLabel,
                    titles = title,
                    xDefault = xDefault, xLimits = xLimits,
                    yDefault = yDefault, yLimits = yLimits,
                    ...)[[1]]
  g <- g + theme_minimal_adapted(xAxis = xAxis, yAxis = yAxis, showLegend = FALSE)
  return(g)
}

#' helper function: function factory to generalize data transformations which
#'  may or may not be needed for volcanoPlot()
#'  
#' @param transformationFormula character vector that specifies the formula
#'  to be used on the argument "data". As an example "log2(data)"
#'  
#' @return a function that takes the argument "data" and transforms it
#' @export
transformData <- function(transformationFormula = "data"){
  function(data){
    return(eval(parse(text = transformationFormula)))
  }
}
#' helper function that specifies the marker lines to add to the volcanoplot()
#' 
#' @return a list of values for the x-axis (vlines) and the y-axis (hlines)
volcanoLineMarkerDedaults <- function(){
  return(list(vlines = log2(c(0.5,1,2)),
              hlines = c(-log10(c(1,0.05)))))
}

#' helper function that specifies the marker line attributes to add to the
#'  volcanoplot()
#' 
#' @return a list of values for the x-axis markerlines (vlinesAttributes) and
#'  the y-axis markerlines (hlinesAttributes)
volcanoMarkerAttributesDefaults <- function(){
  return(list(vlinesAttributes = linesMarkDefaults(),
              hlinesAttributes = linesMarkDefaults()))
}

#' generates a volcano plot of a data.frame with quantification data & 
#'  statisitcal significance data
#' 
#' @param data data.frame with at least two columns
#' @param quantColumn specifies which column in the data argument contains the
#'  quantification data (can be number or character vector)
#' @param statColumn specifies which column in the data argument contains the
#'  statistical significance data (can be number or character vector). Usually
#'  these values are p-values from eg a t-test
#' @param quantTransform specifies the function with which to transform the
#'  quantColumn data (if needed)
#' @param statTransform specifies the function with which to transform the
#'  stattColumn data (if needed)
#'  
#' @note quantColumn is the x-axis, the statColumn is the y-axis
#' 
#' @param xLabel defines x-axis label
#' @param yLabel defines y-axis label
#' @param xCutoffs minimum and maximum value of the quantColumn data
#'  (before transformation). Anything outside these values and lower than
#'  yCutoff will be marked as significant
#' @param yCutoff minimum value of the statColumn data. any value lower is
#'  marked as significant (usually p-value < 0.05)
#' @param pointColor defines the color of the border of the data points
#' @param significantPointColor color of the data points that lie outside the
#'  xCutoff values and below the yCutoff value
#' @param pointAlpha alpha ('see through' value) of the data points
#' @param pointShape shape of the data points
#' @param pointSize size of the data points
#' @param xDefault default is set to TRUE, together with xLimits, this can be
#'  used to define the exact range of the X-axis
#' @param xLimits  default = c(0,NA), together with xDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param yDefault default is set to TRUE, together with yLimits, this can be
#'  used to define the exact range of the Y-axis
#' @param yLimits  default = c(0,NA), together with yDefault, this can be
#'  used to define the exact range of the Y-axis
#' @param xExpand allows for padding around data (x-axis),
#'  see ?ggplot2::expansion for proper explanation
#' @param yExpand allows for padding around data (y-axis),
#'  see ?ggplot2::expansion for proper explanation 
#' @param xSymmetric if TRUE then the range of x-axis will be adjusted to be
#'  equal on both the left and the right side of the center (transformed x = 0)
#' @param xSymmetricExpand allows for padding around data (x-axis), 0.05 means
#'  5 percent (pre-transformation) extra wide x-axis range
#' @param xCentered if TRUE, the plot will be 'cemtered' around the either the
#'  mean or median x-value
#' @param xMedian if TRUE then median and mean absolute deviation (mad) are
#'  used for centering the plot along the x-axis; if FALSE then the mean and tge
#'  standard deviation are used
#' @param xDeviations defines how many deviations the range of the x-axis may
#'  differ from the mean or median. Range will be either (median-xDeviations*mad
#'  ,median+xDeviations**mad) or (mean - xDeviations*sd,mean + xDeviations*sd)
#' @param xLabelFormat allows defining how the x-axis ticks etc are displayed
#'  (see formatDigits() & formatScientificDigits())
#' @param yLabelFormat allows defining how the y-axis ticks etc are displayed
#'  (see formatDigits() & formatScientificDigits())
#' @param vertical if TRUE then plot will be rotated 90 degrees
#' @param title specifies the title
#' @param gridLines if TRUE then gridlines are shown
#' @param volcanoLineMarkers defines where to place marker lines in the plot
#' @param volcanoLineMarkerAttributes defines the attributes of the marker lines
#' @param returnData if TRUE then a list with 2 elements is returned. The first
#'  element is the data.frame used to generate the graph and the second element
#'  is the graph itself
#' @param significanceColumnName name to give to the column specifying if a
#'  row is significant (TRUE) or (FALSE). If not specified, then the name
#'  "significant" will be used
#' @param removeNonSignificantData if TRUE, no non-significant data will be
#'  returned (usually a smaller data.frame to return). If FALSE then all data
#'  will be returned
#' @param identifierColumn specifies which columns from data (names -> character
#'  vector) should be included in the data.frame returned
#' @param ... can be used to pass on other arguments to graphAdjust()
#' 
#' @return a ggplot object or a list  
#' @export
volcanoPlot <- function(data, quantColumn = 1, statColumn = 2,
                        quantTransform = transformData("log2(data)"),
                        statTransform = transformData("-log10(data)"),
                        xLabel = ifelse(is.character(quantColumn),
                                        paste(c("Log2 ",quantColumn), collapse = ""),
                                        paste(c("Log2 ",colnames(data)[quantColumn]), collapse = "")),
                        yLabel = ifelse(is.character(statColumn),
                                        paste(c("-Log10 ",statColumn), collapse = ""),
                                        paste(c("-Log10 ",colnames(data)[statColumn]), collapse = "")),
                        xCutoffs = c(0.5,2), yCutoff = 0.05,
                        pointColor = "black", significantPointColor = "red",
                        pointShape = 21, pointSize = 3, pointAlpha = 0.75,
                        xDefault = TRUE, yDefault = TRUE,
                        xLimits = c(0,NA), yLimits = c(0,NA),
                        xExpand = ggplot2::expansion(mult = 0, add = 0),
                        yExpand = ggplot2::expansion(mult = 0, add = 0),
                        xSymmetric = FALSE, xSymmetricExpand = 0.05,
                        xCentered = FALSE, xMedian = FALSE, xDeviations = 4,
                        xLabelFormat = ggplot2::waiver(), yLabelFormat = ggplot2::waiver(),
                        vertical = FALSE,
                        title ="",
                        gridLines = TRUE,
                        volcanoLineMarkers = volcanoLineMarkerDedaults(),
                        volcanoLineMarkerAttributes = volcanoMarkerAttributesDefaults(),
                        returnData = FALSE,
                        significanceColumnName = "", removeNonSignificantData = TRUE,
                        identifierColumn = NA, ...){
  xColumn <- quantColumn
  yColumn <- statColumn
  if (!is.character(xColumn)){
    xColumn <- colnames(data)[xColumn]
  }
  if (!is.character(yColumn)){
    yColumn <- colnames(data)[yColumn]
  }
  if (identical(identifierColumn,NA)){
    data <- data %>%
      dplyr::select(dplyr::all_of(xColumn), dplyr::all_of(yColumn)) %>%
      stats::na.omit()
  } else {
    if (!is.character(identifierColumn)){
      identifierColumn <- colnames(data)[identifierColumn]
    }
    data <- data %>%
      dplyr::select(dplyr::all_of(identifierColumn), dplyr::all_of(xColumn), dplyr::all_of(yColumn)) %>%
      stats::na.omit()
  }
  data$col <- "1"
  if (nrow(data[(data[,xColumn] <= xCutoffs[1] | data[,xColumn] >= xCutoffs[2]) &
                (data[,yColumn] < yCutoff),]) > 0){
    data[(data[,xColumn] <= xCutoffs[1] | data[,xColumn] >= xCutoffs[2]) &
           (data[,yColumn] < yCutoff),]$col <- "2"
  }
  data$x <- quantTransform(data[,xColumn])
  data$y <- statTransform(data[,yColumn])
  g <- ggplot2::ggplot(data = data, ggplot2::aes_string(x = "x", y = "y", color = "col", fill = "col"))
  g <- g + ggplot2::geom_point(shape = pointShape, size = pointSize, alpha = pointAlpha)
  if (length(unique(data$col)) > 1){
    g <- g + ggplot2::scale_color_manual(values = c(pointColor, significantPointColor))
    g <- g + ggplot2::scale_fill_manual(values = c(pointColor, significantPointColor))
  } else {
    if (data$col[1] == "1"){
      g <- g + ggplot2::scale_color_manual(values = pointColor)
      g <- g + ggplot2::scale_fill_manual(values = pointColor)
    } else {
      g <- g + ggplot2::scale_color_manual(values = significantPointColor)
      g <- g + ggplot2::scale_fill_manual(values = significantPointColor)
    }
  }
  if (xSymmetric){
    if (!xCentered){
      xLimits <- abs(max(c(abs(max(data$x,
                                   na.rm = TRUE)),
                           abs(min(data$x,
                                   na.rm = TRUE))),
                         na.rm = TRUE))
      if (!identical(xSymmetricExpand,NA)){
        xLimits <- xLimits * (1+xSymmetricExpand)
      }
      xLimits <- c(-xLimits,xLimits)
    } else {
      if (xMedian){
        mid <- stats::median(data$x, na.rm = TRUE)
        dev <- stats::mad(data$x, na.rm = TRUE)
      } else {
        mid <- mean(data$x, na.rm = TRUE)
        dev <- stats::sd(data$x, na.rm = TRUE)
      }
      xLimits <- c(mid - (xDeviations*dev), mid + (xDeviations*dev))
    }
    xDefault <- FALSE
  }
  g <- graphsAdjust(list(g), vertical = vertical,
                    xDefault = xDefault, xLimits = xLimits,
                    yDefault = yDefault, yLimits = yLimits,
                    xLabel = xLabel, yLabel = yLabel, titles = title,
                    xLabelFormat = xLabelFormat, yLabelFormat = yLabelFormat,
                    xExpand = xExpand, yExpand = yExpand,
                    setTheme = theme_minimal_adapted(xAxis = TRUE,
                                                     yAxis = TRUE,
                                                     showLegend = FALSE,
                                                     gridLines = gridLines,
                                                     ...),
                    ...)[[1]]
  if (!identical(volcanoLineMarkers,NA)){
    if (!identical(volcanoLineMarkerAttributes,NA)){
      g <- lineMarks(list(g),
                     vlines = volcanoLineMarkers$vlines,
                     hlines = volcanoLineMarkers$hlines,
                     hlinesAttributes = volcanoLineMarkerAttributes$hlinesAttributes,
                     vlinesAttributes = volcanoLineMarkerAttributes$vlinesAttributes)[[1]]
    } else {
      g <- lineMarks(list(g),
                     vlines = volcanoLineMarkers$vlines,
                     hlines = volcanoLineMarkers$hlines)[[1]]
    }
  }
  
  if (!returnData){
    return(g)
  } else {
    data$significant <- FALSE
    if (nrow(data[(data[,xColumn] <= xCutoffs[1] | data[,xColumn] >= xCutoffs[2]) &
                  (data[,yColumn] < yCutoff),]) > 0){
      data[(data[,xColumn] <= xCutoffs[1] | data[,xColumn] >= xCutoffs[2]) &
             (data[,yColumn] < yCutoff),]$significant <- TRUE
      data <- data %>% dplyr::select(-c(col, "x", "y"))
    }
    significant = NULL # solely for the purpose of package
    if (removeNonSignificantData){
      data <- data %>% dplyr::filter(significant)
    }
    if (significanceColumnName != ""){
      colnames(data)[which(colnames(data) == "significant")] <- significanceColumnName
    }
    return(list(graph = g, data = data))
  }
}

#' generates a volcano plot of a data.frame with quantification data & 
#'  statisitcal significance data. Additionally the distributions of the
#'  quantification and the statistical data are shown via density plots (and
#'  optionally a qqplot)
#' 
#' @param data data.frame with at least two columns
#' @param quantColumn specifies which column in the data argument contains the
#'  quantification data (can be number or character vector)
#' @param statColumn specifies which column in the data argument contains the
#'  statistical significance data (can be number or character vector). Usually
#'  these values are p-values from eg a t-test
#' @param xLabel defines x-axis label
#' @param yLabel defines y-axis label
#' @param xCutoffs minimum and maximum value of the quantColumn data
#'  (before transformation). Anything outside these values and lower than
#'  yCutoff will be marked as significant
#' @param yCutoff minimum value of the statColumn data. any value lower is
#'  marked as significant (usually p-value < 0.05)
#' @param title specifies the title
#' @param gridLines if TRUE then gridlines are shown
#' @param volcanoLineMarkers defines where to place marker lines in the plot
#' @param volcanoLineMarkerAttributes defines the attributes of the marker lines
#' @param returnData if TRUE then a list with 2 elements is returned. The first
#'  element is the data.frame used to generate the graph and the second element
#'  is the graph itself
#' @param significanceColumnName name to give to the column specifying if a
#'  row is significant (TRUE) or (FALSE). If not specified, then the name
#'  "significant" will be used
#' @param removeNonSignificantData if TRUE, no non-significant data will be
#'  returned (usually a smaller data.frame to return). If FALSE then all data
#'  will be returned
#' @param identifierColumn specifies which columns from data (names -> character
#'  vector) should be included in the data.frame returned
#' @param showQQPlot if TRUE then a qq plot of the x-axis (after transformation)
#'  is shown in the left bottom corner
#' @param widths horizontal: two number (integer) vector specifying the amount
#'  of the plot to be used for the volcanoplot and the amount for the density
#'  plot
#' @param heights vertical: two number (integer) vector specifying the amount of
#'  the plot to be used for the volcanoplot and the amount for the density plot
#' @param ... can be used to pass on other arguments to graphAdjust()
#' 
#' @return a ggplot object or a list
#' 
#' @note what is returned is grobtable (class gtable): this can be drawn via
#'  the grid.draw() function. The plot drawn before this will have to be cleared
#'  "manually" via clearPlot()
#' 
#' @export
volcanoPlotPlus <- function(data, quantColumn = 1, statColumn = 2,
                    xLabel = ifelse(is.character(quantColumn),
                                    paste(c("Log2 ",quantColumn), collapse = ""),
                                    paste(c("Log2 ",colnames(data)[quantColumn]), collapse = "")),
                    yLabel = ifelse(is.character(statColumn),
                                    paste(c("-Log10 ",statColumn), collapse = ""),
                                    paste(c("-Log10 ",colnames(data)[statColumn]), collapse = "")),
                    xCutoffs = c(0.5,2), yCutoff = 0.05,
                    title ="",
                    gridLines = TRUE,
                    volcanoLineMarkers = volcanoLineMarkerDedaults(),
                    volcanoLineMarkerAttributes = volcanoMarkerAttributesDefaults(),
                    returnData = FALSE,
                    significanceColumnName = "", removeNonSignificantData = TRUE,
                    identifierColumn = NA,
                    showQQPlot = TRUE,
                    widths = c(125,875), heights = c(875,125), ...){
  result <- volcanoPlot(data = data, 
                        quantColumn = quantColumn,
                        statColumn = statColumn,
                        xCutoffs = xCutoffs, yCutoff = yCutoff,
                        identifierColumn = identifierColumn,
                        returnData = returnData,
                        xSymmetric = TRUE,
                        title = title,
                        gridLines = gridLines,
                        volcanoLineMarkers = volcanoLineMarkers)
  if (!returnData){
    result <- list(result)
    names(result) <- "VolcanoPlot"  
  } else {
    names(result)[1] <- "VolcanoPlot"
  }
  densityPlotX <- statDensity(log2(data[,quantColumn]),
                              yExpand = c(0,0,0.05,0),
                              fillColor = "lightgrey",
                              outlineColor = "darkgrey",
                              xLabel = xLabel, title = NULL,
                              xSymmetric = TRUE,
                              xAxis = FALSE, yAxis = FALSE,
                              gridLinesY = FALSE) %>%
    lineMarks(vlines = volcanoLineMarkers$vlines, hlines = NA,
              vlinesAttributes = volcanoLineMarkerAttributes$vlinesAttributes,
              hlinesAttributes = NA)
  densityPlotY <- statDensity(-log10(data[,statColumn]),
                              yExpand = c(0,0,0.05,0),
                              fillColor = "lightgrey",
                              outlineColor = "darkgrey",
                              title = NULL,
                              xSymmetric = FALSE,
                              vertical = TRUE,
                              gridLinesX = FALSE,
                              xAxis = FALSE, yAxis = FALSE) %>%
    lineMarks(vlines = volcanoLineMarkers$hlines, hlines = NA,
              vlinesAttributes = volcanoLineMarkerAttributes$hlinesAttributes,
              hlinesAttributes = NA)
  nqqPlotX <- normalQuantilePlot(log2(data[, quantColumn]),
                                 pointSize = 0.25,
                                 pointFill = "red",
                                 pointColor = "red",
                                 pointAlpha = 0.5,
                                 lineColor = "blue",
                                 lineWidth =  0.75,
                                 lineType = "solid",
                                 lineAlpha = 0.5,
                                 xAxis = FALSE, yAxis = FALSE) 
  aligned <- cowplot::align_plots(result[[1]], densityPlotX, align = "v")
  aligned2 <- cowplot::align_plots(densityPlotY, result[[1]], align = "h")
  if (showQQPlot){
    result[[1]] <- gridExtra::arrangeGrob(grobs = list(aligned2[[1]],
                                             aligned2[[2]],
                                             nqqPlotX,
                                             aligned[[2]]),
                                ncol = 2, nrow = 2,
                                widths = widths, heights = heights)
  } else {
    result[[1]] <- gridExtra::arrangeGrob(grobs = list(aligned2[[1]],
                                             aligned2[[2]],
                                             ggplot2::ggplot() + ggplot2::theme_minimal(),
                                             aligned[[2]]),
                                ncol = 2, nrow = 2,
                                widths = widths, heights = heights)
  }
  if (!returnData){
    result <- result[[1]]
  }
  return(result)
}

#' helper function to clear a plot area or to generate an empty ggplot object
#' 
#' @return empty ggplot object
#' @export
clearPlot <- function(){
  ggplot2::ggplot()+ggplot2::theme_void()
}

#' generates a ggplot object of a scatterplot
#' 
#' @param data the data tp be plotted, data.frame or similar
#' @param xColumn specifies which column in the data argument contains the
#'  x-data (refer to column via number or character vector (column name))
#' @param yColumn specifies which column in the data argument contains the
#'  y-data (refer to column via number or character vector (column name))
#' @param xTransform specifies the function with which to transform the
#'  x-data (if needed), use transformData() for this
#' @param yTransform specifies the function with which to transform the
#'  y-data (if needed), use transformData() for this
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. @note this has consquence that ROWS will be removed when using
#'  multiple columns with data.frame's
#' @param xLabel defines x-axis label
#' @param yLabel defines y-axis label
#' @param pointColor defines the color of the border of the data points
#' @param pointFill defines the color of the data points themselves
#' @param pointAlpha alpha ('see through' value) of the data points
#' @param pointShape shape of the data points
#' @param pointSize size of the data points
#' @param smoothLine if TRUE then a smoothing line is drawn (via geom_smooth())
#' @param smoothLineColor color of the smoothing line
#' @param smoothLineType type of the smoothing line
#' @param smoothWidth width of the smoothing nline
#' @param smoothAlpha alpha ('see through' value) of the smoothing line
#' @param smoothOrientation eihter "x" or "y", specifies what the smoothing
#'  orientation should be. See ?ggplot2::geom_smooth for more information
#' @param smoothConfidence if TRUE then confidence 'band' is drawn around the
#'  smoothing line
#' @param smoothFill specifies fill color pf tje smoothing confidence 'band'
#' @param smoothMethod defines smoothin method. See ?ggplot2::geom_smooth for
#'  more info
#' @param title specifies the title
#' @param vertical if TRUE then the plot is rotated 90 degrees (swap of X & Y)
#' @param xAxis if TRUE, the x-axis is properly draw with label, ticks etc
#' @param xDefault if default is set to FALSE, then together with xLimits, this
#'  can be used to define the exact range of the x-axis
#' @param xLimits  default = c(0,NA), together with xDefault, this can be
#'  used to define the exact range of the x-axis
#' @param xLog if TRUE then logarithmic scale is used for the x-axis
#' @param xSymmetric if TRUE then the range of x-axis will be adjusted to be
#'  equal on both the left and the right side of the center
#' @param xSymmetricExpand allows for padding around data (x-axis), 0.05 means
#'  5 percent extra wide x-axis range
#' @param xCentered if TRUE, the plot will be 'cemtered' around the either the
#'  mean or median x-value
#' @param xMedian if TRUE then median and mean absolute deviation (mad) are
#'  used for centering the plot along the x-axis; if FALSE then the mean and the
#'  standard deviation are used
#' @param xDeviations defines how many deviations the range of the x-axis may
#'  differ from the mean or median. Range will be either (median-xDeviations*mad
#'  ,median+xDeviations**mad) or (mean - xDeviations*sd,mean + xDeviations*sd)
#' @param yAxis if TRUE, the y-axis is properly draw with label, ticks etc
#' @param yDefault if default is set to FALSE, then together with yLimits, this
#'  can be used to define the exact range of the y-axis
#' @param yLimits  default = c(0,NA), together with yDefault, this can be
#'  used to define the exact range of the y-axis
#' @param yLog if TRUE then logarithmic scale is used for the y-axis
#' @param ySymmetric if TRUE then the range of y-axis will be adjusted to be
#'  equal on both the left and the right side of the center
#' @param ySymmetricExpand allows for padding around data (y-axis), 0.05 means
#'  5 percent extra wide y-axis range
#' @param yCentered if TRUE, the plot will be 'cemtered' around the either the
#'  mean or median y-value
#' @param yMedian if TRUE then median and mean absolute deviation (mad) are
#'  used for centering the plot along the y-axis; if FALSE then the mean and the
#'  standard deviation are used
#' @param yDeviations defines how many deviations the range of the x-axis may
#'  differ from the mean or median. Range will be either (median-xDeviations*mad
#'  ,median+xDeviations**mad) or (mean - xDeviations*sd,mean + xDeviations*sd)
#' @param gridLines if TRUE then gridlines are shown
#' @param gridLinesX if TRUE then vertical gridlines are shown (set gridLines to
#'  FALSE when using this)
#' @param gridLinesY if TRUE then horizontal gridlines are shown (set gridLines
#'  to FALSE when using this)
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  
#' @return a ggplot object
#' @export
scatterPlot <- function(data, xColumn = 1, yColumn = 2,
                        xTransform = NA, yTransform = NA,
                        removeNA = FALSE,
                        xLabel = ifelse(is.character(xColumn),
                                        xColumn,
                                        colnames(data)[xColumn]),
                        yLabel = ifelse(is.character(yColumn),
                                        yColumn,
                                        colnames(data)[yColumn]),
                        pointAlpha = 0.75, pointColor = "black",
                        pointFill = "red", pointShape = 21,
                        pointSize = 2,
                        smoothLine = FALSE,
                        smoothLineType = "solid", smoothLineColor = "black",
                        smoothAlpha = 0.1, smoothFill = "lightblue",
                        smoothWidth = 0.5, smoothOrientation = "x",
                        smoothConfidence = FALSE, smoothMethod = NULL,
                        title = paste(c(yLabel, " vs ", xLabel),
                                      collapse = ""),
                        vertical = FALSE,
                        xAxis = TRUE,
                        xDefault = TRUE,
                        xLimits = c(0,NA), xLog = FALSE,
                        xSymmetric = FALSE, xSymmetricExpand = 0.05,
                        xCentered = TRUE, xMedian = FALSE, xDeviations = 4,
                        yAxis = TRUE,
                        yDefault = TRUE,
                        yLimits = c(0,NA), yLog = FALSE,
                        ySymmetric = FALSE, ySymmetricExpand = 0.05,
                        yCentered = TRUE, yMedian = FALSE, yDeviations = 4,
                        gridLines = TRUE,
                        gridLinesX = TRUE,
                        gridLinesY = TRUE,
                        ...){
  if (!is.character(xColumn)){
    xColumn <- colnames(data)[xColumn]
  }
  if (!is.character(yColumn)){
    yColumn <- colnames(data)[yColumn]
  }
  data <- data %>%
    dplyr::select(dplyr::all_of(xColumn), dplyr::all_of(yColumn))
  if (removeNA){
    data <- data %>% stats::na.omit()
  }
  if (!identical(xTransform, NA)){
    data[,xColumn] <- xTransform(data[,xColumn])
  }
  if (!identical(yTransform, NA)){
    data[,yColumn] <- yTransform(data[,yColumn])
  }
  g <- ggplot2::ggplot(data = data, (ggplot2::aes(x = !!dplyr::sym(xColumn), y = !!dplyr::sym(yColumn)))) +
    ggplot2::geom_point(alpha = pointAlpha, color = pointColor,
               fill = pointFill, shape = pointShape,
               size = pointSize)
  if (smoothLine){
    g <- g + ggplot2::stat_smooth(color = smoothLineColor,
                         fill = smoothFill,
                         linetype = smoothLineType,
                         size = smoothWidth,
                         se = smoothConfidence,
                         na.rm = removeNA,
                         method = smoothMethod,
                         orientation = smoothOrientation)
  }
  if (xSymmetric){
    if (!xCentered){
      xLimits <- abs(max(c(abs(max(data[,xColumn],
                                   na.rm = TRUE)),
                           abs(min(data[,xColumn],
                                   na.rm = TRUE))),
                         na.rm = TRUE))
      if (!identical(xSymmetricExpand,NA)){
        xLimits <- xLimits * (1+xSymmetricExpand)
      }
      xLimits <- c(-xLimits,xLimits)
    } else {
      if (xMedian){
        mid <- stats::median(data[, xColumn], na.rm = TRUE)
        dev <- stats::mad(data[,xColumn], na.rm = TRUE)
      } else {
        mid <- mean(data[, xColumn], na.rm = TRUE)
        dev <- stats::sd(data[,xColumn], na.rm = TRUE)
      }
      xLimits <- c(mid - (xDeviations*dev), mid + (xDeviations*dev))
    }
    xDefault <- FALSE
  }
  if (ySymmetric){
    if (!xCentered){
      yLimits <- abs(max(c(abs(max(data[,yColumn],
                                   na.rm = TRUE)),
                           abs(min(data[,yColumn],
                                   na.rm = TRUE))),
                         na.rm = TRUE))
      if (!identical(ySymmetricExpand,NA)){
        yLimits <- yLimits * (1+ySymmetricExpand)
      }
      yLimits <- c(-yLimits,yLimits)
    } else {
      if (yMedian){
        mid <- stats::median(data[,yColumn], na.rm = TRUE)
        dev <- stats::mad(data[,yColumn], na.rm = TRUE)
      } else {
        mid <- mean(data[,yColumn], na.rm = TRUE)
        dev <- stats::sd(data[,yColumn], na.rm = TRUE)
      }
      yLimits <- c(mid - (yDeviations*dev), mid + (yDeviations*dev))
    }
    yDefault <- FALSE
  }
  g <- graphsAdjust(list(g),
                    vertical = vertical,
                    xLabel = xLabel, xDefault = xDefault, xLimits = xLimits,
                    yLabel = yLabel, yDefault = yDefault, yLimits = yLimits,
                    xLog = xLog, yLog = yLog,
                    titles = title,
                    setTheme = theme_minimal_adapted(xAxis = xAxis,
                                                     yAxis = yAxis,
                                                     showLegend = FALSE,
                                                     gridLines = gridLines,
                                                     gridLinesX = gridLinesX,
                                                     gridLinesY = gridLinesY),
                    ...)[[1]]
  return(g)
}

#' generates a variant of the scatterplot when one needs to compare two sets
#'  of data (x & y): Bland-Altman / Tukey mean-difference plot
#'  
#' @param data the data tp be plotted, data.frame or similar
#' @param xColumn specifies which column in the data argument contains the
#'  x-data (refer to column via number or character vector (column name))
#' @param yColumn specifies which column in the data argument contains the
#'  y-data (refer to column via number or character vector (column name))
#' @param logTransform do logaithmic transformation o/t data
#' @param log2Transform if TRUE do log2 transformation o/t data in stead of
#'  log10 (if logTransform = TRUE!)
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to plotting. Note: this has consquence that ROWS will be removed when using
#'  multiple columns with data.frame's
#' @param xLabel defines x-axis label
#' @param yLabel defines y-axis label
#' @param title specifies the title
#' @param ... can be used to pass on other arguments to graphAdjust()
#'  
#' @return a ggplot object
#' 
#' @note work in progress...
#' 
#' @export
scatterBlandAltman <- function(data, xColumn = 1, yColumn = 2,
                               logTransform = FALSE, log2Transform = TRUE,
                               removeNA = FALSE,
                               xLabel = ifelse(logTransform,
                                               ifelse(log2Transform,
                                                      "Average of log2(x,y)",
                                                      "Average of log10(x,y)"),
                                               "Average of c(x,y)"),
                               yLabel = ifelse(logTransform,
                                               ifelse(log2Transform,
                                                      "Difference of log2(x,y)",
                                                      "Difference of log10(x,y)"),
                                               "Difference of c(x,y)"),
                               title = paste(c("Bland-Altman plot/Tukey mean-difference plot: ", yLabel, " vs ", xLabel),
                                             collapse = ""),
                              ...){
  if (!is.character(xColumn)){
    xColumn <- colnames(data)[xColumn]
  }
  if (!is.character(yColumn)){
    yColumn <- colnames(data)[yColumn]
  }
  if (removeNA){
    data <- data %>% stats::na.omit()
  }
  if (xColumn != yColumn){
    data <- data %>%
      dplyr::select(dplyr::all_of(xColumn), dplyr::all_of(yColumn))
  } else {
    data <- data.frame(x = data[, xColumn], y = data[, xColumn])
    yColumn <- paste0(yColumn,"__")
    colnames(data) <- c(xColumn, yColumn)
  }
  data2 <- data
  if (!logTransform){
    data[,xColumn] <- (data2[,xColumn] + data2[,yColumn])/2 # average
    data[,yColumn] <- data2[,xColumn] - data2[,yColumn]     # difference
  } else {
    if (log2Transform){
      transformer <- transformData("log2(data)")
    } else {
      transformer <- transformData("log10(data)")
    }
    data[,xColumn] <- (transformer(data2[,xColumn]) + transformer(data2[,yColumn]))/2
    data[,yColumn] <- transformer(data2[,xColumn]) - transformer(data2[,yColumn])
  }
  scatterPlot(data = data, xColumn = xColumn, yColumn = yColumn,
              removeNA = removeNA,
              xLabel = xLabel, yLabel = yLabel, title = title, ...)
}

#' function to generate an aligned set of (maximum 4) plots as a 2x2 matrix
#'  (2 columns, 2 rows)
#'  
#' @param sPlot ggplot object to be placed in top, right corner
#' @param xPlot ggplot object to be placed in bottom, right corner (along
#'  x-axis of sPlot object)
#' @param yPlot ggplot object to be placed in top, left corner (along y-axis of
#'  sPlot object)
#' @param crossPlot ggplot object to be placed in bottom, left corner
#' @param widths horizontal: two number (integer) vector specifying the amount
#'  of the plot to be used for the plots (horizontally)
#' @param heights vertical: two number (integer) vector specifying the amount of
#'  the plot to be used for the plots (vertically)
#'  
#' @return a ggplot object
#' 
#' @note what is returned is grobtable (class gtable): this can be drawn via
#'  the grid.draw() function. The plot drawn before this will have to be cleared
#'  "manually" via clearPlot()
#' 
#' @export
plotPlusMatrix <- function(sPlot,
                           xPlot = clearPlot(),
                           yPlot = clearPlot(),
                           crossPlot = clearPlot(),
                           widths = c(125,875), heights = c(875,125)){
  aligned <- cowplot::align_plots(sPlot, xPlot, align = "v")
  aligned2 <- cowplot::align_plots(yPlot, sPlot, align = "h")
  result <- gridExtra::arrangeGrob(grobs = list(aligned2[[1]],
                                                aligned2[[2]],
                                                crossPlot,
                                                aligned[[2]]),
                                   ncol = 2, nrow = 2,
                                   widths = widths, heights = heights)
  return(result)
}

#' function to generate an aligned set of (maximum 3) plots as a 1x3 matrix
#'  (3 columns, 1 row)
#'  
#' @param sPlot ggplot object to be placed in the middle
#' @param yLeft ggplot object to be placed on the left (along y-axis of sPlot)
#' @param yRight ggplot object to be placed on the right (along y-axis of sPlot)
#' @param widths horizontal: two number (integer) vector specifying the amount
#'  of the plot to be used for the plots (horizontally)
#'  
#' @return a ggplot object
#' 
#' @note what is returned is grobtable (class gtable): this can be drawn via
#'  the grid.draw() function. The plot drawn before this will have to be cleared
#'  "manually" via clearPlot()
#' 
#' @export
scatterPlotPlusH3 <- function(sPlot,
                              yLeft = clearPlot(),
                              yRight = clearPlot(),
                              widths = c(150,500,350)){
  aligned <- cowplot::align_plots(yLeft, sPlot, align = "h")
  aligned2 <- cowplot::align_plots(sPlot, yRight, align = "h")
  result <- gridExtra::arrangeGrob(grobs = list(aligned[[1]],
                                                aligned[[2]],
                                                aligned2[[2]]),
                                   ncol = 3,
                                   widths = widths)
  return(result)
}
