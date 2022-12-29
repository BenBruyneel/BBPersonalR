#' to be able to use the theme theme_minimal with some adjustments
#' 
#' @param base_size size of the lettering  of axis, title etc
#' @param base_family letter type of axis, title etc
#' @param base_line_size width of gridlines, set  to 0 for none
#' @param base_rect_size width of axis lines, set to 0 for none
#' @param xAxis if TRUE then display xAxis title
#' @param yAxis if TRUE then display yAxis title
#' @param showLegend if TRUE then show legend
#' @param legend.position defines where to place the legend
#' @param gridLines if TRUE then display gridlines
#' @param gridLinesX if TRUE then display gridlines 'along' the x-axis
#' @param gridLinesY if TRUE then display gridlines 'along' the y-axis
#' @param titleSize if NA, use default title size, else use titleSize value
#' 
#' To be used as ggplot-object + theme_minimal_adapted() 
#' 
#' @returns theme definition 
#' @export
theme_minimal_adapted <- function(base_size = 11, base_family = "",
                                  base_line_size = base_size/22,
                                  base_rect_size = base_size/22,
                                  xAxis = TRUE, yAxis = TRUE,
                                  showLegend = TRUE, legend.position = "bottom",
                                  gridLines = TRUE,
                                  gridLinesX = TRUE,
                                  gridLinesY = TRUE,
                                  titleSize = NA){
  theTheme <- ggplot2::theme_bw(base_size = base_size, base_family = base_family, 
                       base_line_size = base_line_size,
                       base_rect_size = base_rect_size) %+replace% 
    ggplot2::theme(axis.ticks = ggplot2::element_blank(), legend.background = ggplot2::element_blank(), 
          legend.key = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
          strip.background = ggplot2::element_blank(), #panel.border = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(), complete = TRUE)
  if (!xAxis) {
    theTheme <- theTheme %+replace%
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
  }
  if (!yAxis) {
    theTheme <- theTheme %+replace%
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
  }
  if (showLegend){
    theTheme <- theTheme %+replace%
      ggplot2::theme(legend.position = legend.position)
  } else {
    theTheme <- theTheme %+replace%
      ggplot2::theme(legend.position = "none")
  }
  if (!gridLines){
    theTheme <- theTheme %+replace%
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  } else {
    if (!gridLinesX){
      theTheme <- theTheme %+replace%
        ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
              panel.grid.minor.x = ggplot2::element_blank())
    }
    if (!gridLinesY){
      theTheme <- theTheme %+replace%
        ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
              panel.grid.minor.y = ggplot2::element_blank())
    }
  }
  if (!identical(titleSize,NA)){
    theTheme <- theTheme %+replace%
      ggplot2::theme(plot.title = ggplot2::element_text(size = titleSize))
  }
  return(theTheme)
}

#' Function to convert numerical vectors into strings. This specific one can be
#'  used in stead of the function factories below if needed when no options are
#'  needed
#' @param v numeric vector to be converted to a character vector
#'  
#' @return a character vector
#'  
#' @export
formatDigitsWaiver <- function(v){
  return(as.character(v))
}

#' Function factoru to be used to specify the number of digits to be used
#'  in numbers
#'
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'  
#' @returns a function that will take a numeric vector as an argument and 
#'  returns a character vector of the numeric vector with the set number of
#'  digits (see ?scales::lebel_number for more info)
#'  
#' @note this is more or less an example of a function to be used to specify
#'  axis-label formats with the function graphAdjust(). More complex functions
#'  are possible
#' @export
formatDigits <- function(digits){
  function(v){
    return(scales::comma(v, accuracy = 10^(-digits)))
  }
}

#' Function factoru to be used to specify the number of digits to be used
#'  in large numbers. The function generates numbers as strings w/o big marks
#'  (in US/UK commas)
#'
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'  
#' @returns a function that will take a numeric vector as an argument and 
#'  returns a character vector of the numeric vector with the set number of
#'  digits (see ?scales::lebel_number for more info) but w/o big marks
#'  
#' @export
formatDigitsLargeNumbers <- function(digits){
  function(v){
    return(scales::comma(v, accuracy = 10^(-digits), big.mark = ""))
  }
}

#' Function factoru to be used to specify the number of digits to be used
#'  in numbers when using scientific notation
#'  
#' @param digits integer value that specifies the number of digits to be used
#'  by the resulting function
#'  
#' @returns a function that will take a numeric vector as an argument and 
#'  returns a character vector of the numeric vector in scientific format with
#'  the set number of digits (see ?scales::lebel_scientific for more info)
#'  
#' @note this is more or less an example of a function to be used to specify
#'  axis-label formats with the function graphAdjust(). More complex functions
#'  are possible
#' @export
formatScientificDigits <- function(digits){
  function(v){
    return(scales::scientific(v, digits = digits))
  }
}

#' Make adjustments to a graph eg zoom, titles etc etc
#' 
#' @param graphs list of ggplot-objects to which the adjustments have to be made
#'  Note: MUST be a list
#' @param vertical if TRUE, flips x- and y-axis
#' @param xDiscrete specifies whether the x-axis should be discrete
#' @param yDiscrete specifies whether the y-axis should be discrete
#' @param xReverse specifies whether to reverse the x-axis
#' @param yReverse specifies whether to reverse the y-axis
#' @param xDefault if TRUE, then xExpand, xLimits and xOob are ignored
#'  (essentially autoscaling the x-axis)
#' @param yDefault same as xDefault, but for y-axis
#' @param xLimits range of the x-axis, normally xLimits = c(minimum, maximum)
#' if minimum and/or maximum is NA, then they are autoscaled, if xLimits = NA
#'  then the range is 0, putting all datapoints in one line (x-axis-wise)
#' @param yLimits range of the y-axis (see xLimits)
#' @param xExpand allows for padding around data (x-axis),
#'  see ?ggplot2::expansion for proper explanation
#' @param yExpand allows for padding around data (y-axis),
#'  see ?ggplot2::expansion for proper explanation 
#' @param xLabelFormat defines the numeric format to be used for the x-axis
#'  labels (see fromatDigits() & formatScientificDigits() for examples)
#' @param yLabelFormat defines the numeric format to be used for the y-axis
#'  labels (see fromatDigits() & formatScientificDigits() for examples)
#' @param xOob defines what to do with data that's out of range of the data,
#'  see ?scales::oob for proper explanation. Note: only deals with x-axis
#' @param yOob defines what to do with data that's out of range of the data,
#'  see ?scales::oob for proper explanation. Note: only deals with y-axis
#' @param xLog if TRUE then automatic transformation of the x-axis to logarihmic
#'  scale
#' @param yLog if TRUE then automatic transformation of the y-axis to logarihmic
#'  scale
#' @param xIsDate if TRUE then all settings regarding the x-axis are ignored,
#'  except xLimits which should be dates (defaults are not dates). Alternatively
#'  xDefault can be set to TRUE for autoscaling
#' @param yIsDate if TRUE then all settings regarding the y-axis are ignored 
#'  except yLimits which should be dates (defaults are not dates). Alternatively
#'  xDefault can be set to TRUE for autoscaling
#' @param titles sets title of graph
#' @param xLabel sets x-axis title
#' @param yLabel set y-axos title
#' @param setTheme if NA, then no theme is applied, otherwise uses the defined
#'  theme (can also be ggplot2 included themes, such as theme_bw())
#' @param plot.margins.default if TRUE, ignore plot.margins and other parameters
#' @param plot.margins defines margins (from the border) of the plot
#'  c(top, right, bottom, left)   
#' @param plot.margins.units default = "points", other possibilities:
#'  ?grid::unit , examples "cm", "points", "inches", "npc" (viewport) etc etc
#'  
#' @returns a list of ggplot objects
#' @export
graphsAdjust <- function(graphs, vertical = FALSE,
                         xDiscrete = FALSE, yDiscrete = FALSE,
                         xReverse = FALSE, yReverse = FALSE,
                         xDefault = FALSE, yDefault = FALSE, 
                         xLimits = c(0,NA), yLimits = c(0,NA),
                         xExpand = ggplot2::expansion(mult = 0, add = 0),
                         yExpand = ggplot2::expansion(mult = 0, add = 0),
                         xLabelFormat = ggplot2::waiver(), yLabelFormat = ggplot2::waiver(),
                         xOob = scales::oob_squish_infinite,
                         yOob = scales::oob_squish_infinite,
                         xLog = FALSE, yLog = FALSE,
                         xIsDate = FALSE, yIsDate = FALSE,
                         titles = NA, xLabel = NA, yLabel = NA,
                         setTheme = theme_minimal_adapted(),
                         plot.margins.default = TRUE,
                         plot.margins = c(5,5,5,5),
                         plot.margins.units = "points"){
  if (xLog){
    if (xReverse){
      xReverse <- ggforce::trans_reverser('log10')
      xLimits <- xLimits[2:1]
    }  else {
      xReverse <- scales::log10_trans()
    }
  }
  if (yLog){
    if (yReverse){
      yReverse <- ggforce::trans_reverser('log10')
      yLimits <- xLimits[2:1]
    }  else {
      yReverse <- scales::log10_trans()
    }
  }
  for (counter in 1:(length(graphs))){
    # axis transformations
    if (!xIsDate){
      if (xLog) {
        if (!xDefault){
          graphs[[counter]] <- graphs[[counter]] + 
            ggplot2::scale_x_continuous(expand = xExpand,
                               limits = xLimits,
                               oob = xOob,
                               trans = xReverse,
                               labels = xLabelFormat)
        } else {
          graphs[[counter]] <- graphs[[counter]] + 
            ggplot2::scale_x_continuous(labels = xLabelFormat, trans = xReverse)
        }
      } else {
        if (!xDefault){
          if (!xReverse){
            graphs[[counter]] <- graphs[[counter]] + 
              ggplot2::scale_x_continuous(expand = xExpand, limits = xLimits, oob = xOob,
                                 labels = xLabelFormat)
          } else {
            graphs[[counter]] <- graphs[[counter]] + 
              ggplot2::scale_x_reverse(expand = xExpand, limits = xLimits[2:1], oob = xOob,
                                 labels = xLabelFormat)
          }
        } else {
          if (!xDiscrete){
            if (!xReverse){
              graphs[[counter]] <- graphs[[counter]] + 
                ggplot2::scale_x_continuous(labels = xLabelFormat)
            } else {
              graphs[[counter]] <- graphs[[counter]] + 
                ggplot2::scale_x_reverse(labels = xLabelFormat)
            }
          } else {
            if (!xReverse){
              graphs[[counter]] <- graphs[[counter]] + 
                ggplot2::scale_x_discrete(labels = xLabelFormat)
            } else {
              graphs[[counter]] <- graphs[[counter]] + 
                ggplot2::scale_x_discrete(limits = rev, labels = xLabelFormat)
            }
          }
        }
      }
    } else {
      if (!xDefault){
        graphs[[counter]] <- graphs[[counter]] +
          ggplot2::scale_x_datetime(limits = xLimits,
                       labels = xLabelFormat)#, trans = xReverse)  -- do not know if works
      }
    }
    if (!yIsDate){
      if (yLog) {
        if (!yDefault){
          graphs[[counter]] <- graphs[[counter]] + 
            ggplot2::scale_y_continuous(expand = yExpand,
                               limits = yLimits,
                               oob = yOob,
                               trans = yReverse,
                               labels = yLabelFormat)
        } else {
          graphs[[counter]] <- graphs[[counter]] + 
            ggplot2::scale_y_continuous(labels = yLabelFormat, trans = yReverse)
        }
      } else {
        if (!yDefault){
          if (!yReverse){
            graphs[[counter]] <- graphs[[counter]] + 
              ggplot2::scale_y_continuous(expand = yExpand, limits = yLimits, oob = yOob,
                                 labels = yLabelFormat)
          } else {
            graphs[[counter]] <- graphs[[counter]] + 
              ggplot2::scale_y_reverse(expand = yExpand, limits = yLimits[2:1], oob = yOob,
                              labels = yLabelFormat)
          }
        } else {
          if (!yDiscrete){
            if (!yReverse){
              graphs[[counter]] <- graphs[[counter]] + 
                ggplot2::scale_y_continuous(labels = yLabelFormat)
            } else {
              graphs[[counter]] <- graphs[[counter]] + 
                ggplot2::scale_y_reverse(labels = yLabelFormat)
            }
          } else {
            if (!yReverse){
              graphs[[counter]] <- graphs[[counter]] + 
                ggplot2::scale_y_discrete(labels = yLabelFormat)
            } else {
              graphs[[counter]] <- graphs[[counter]] + 
                ggplot2::scale_y_discrete(limits = rev, labels = yLabelFormat)
            }
          }
        }
      }
    } else {
      if (!yDefault){
        graphs[[counter]] <- graphs[[counter]] +
          ggplot2::scale_y_datetime(limits = yLimits,
                                    labels = yLabelFormat)#, trans = xReverse)  -- do not know if works
      }
    }
    # swap x & y
    if (vertical){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::coord_flip()
    }
    # labels
    if (!identical(titles,NA)){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::ggtitle(titles[counter])
    }
    if (!identical(xLabel,NA)){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::xlab(xLabel)
    }
    if (!identical(yLabel,NA)){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::ylab(yLabel)
    }
    if (!identical(setTheme,NA)){
      graphs[[counter]] <- graphs[[counter]] + setTheme
    }
    if (!plot.margins.default){
      graphs[[counter]] <- graphs[[counter]] + 
        ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
    }
  }
  return(graphs)
}


#' helper function to provide line properties
#' 
#' @param color color of the line
#' @param linetype linetype (solid, dotted, etc) of the line
#' @param size width of the line
#' @param alpha alpha ('see through' factor) of the line
#' 
#' @returns a list object of parameters
#' @export
lineAttributes <- function(size = 1,
                           linetype = "solid",
                           color = "black",
                           alpha = 1){
  return(list(size = size,
              linetype = linetype,
              color = color,
              alpha = alpha))
}

#' helper function to provide line properties for marks
#' 
#' @note uses lineAttributes function
#' 
#' @returns a list object of parameters
#' @export
linesMarkDefaults <- function(){
  return(lineAttributes(size = 0.5,
                        linetype = "dashed",
                        color = "red",
                        alpha = 1))
}

#' to add horizontal or vertical lines to a list of ggplot objects
#'
#' @param graphs a ggplot object or a list of ggplot-objects to which the lines
#'  have to be added
#' @param vlines a numeric vector defining where the vertical lines are to be
#'  placed
#' @param hlines  a numeric vector defining where the vertical lines are to be
#'  placed
#' @param vlinesAttributes list which defines the type of line to be used for
#'  vlines parameter
#' @param hlinesAttributes list which defines the type of line to be used for
#'  hlines parameter
#'  
#' @note due to the graphs parameter you can use:
#'  list(ggplot objects) %>% graphAdjust() %>% lineMarks()
#'   
#' @returns a list of ggplot objects
#' @export
lineMarks <- function(graphs,
                      vlines = NA,
                      hlines = NA,
                      vlinesAttributes = linesMarkDefaults(),
                      hlinesAttributes = linesMarkDefaults()){
  if (identical(vlines, NA) & (identical(hlines, NA))){
    return(graphs)
  }
  notList <- !is.Class(graphs, "list")
  if (notList){
    graphs <- list(graphs)
  }
  for (counter in 1:(length(graphs))){
    if ((!identical(vlines,NA))){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::geom_vline(xintercept = vlines,
                                                          size = vlinesAttributes$size,
                                                          linetype = vlinesAttributes$linetype,
                                                          color = vlinesAttributes$color,
                                                          alpha = vlinesAttributes$alpha
      )
    }
    if ((!identical(hlines,NA))){
      graphs[[counter]] <- graphs[[counter]] + ggplot2::geom_hline(yintercept = hlines,
                                                          size = hlinesAttributes$size,
                                                          linetype = hlinesAttributes$linetype,
                                                          color = hlinesAttributes$color,
                                                          alpha = hlinesAttributes$alpha)
    }
  }
  if (notList){
    graphs <- graphs[[1]]
  }
  return(graphs)
}

#' helper function to provide area properties
#' 
#' @param color color of the line around the area
#' @param fillColor color of the area
#' @param linetype linetype (solid, dotted, etc) of the line around the area
#' @param size width of the line around the area
#' @param alpha alpha ('see through' factor) of the area
#' 
#' @returns a list object of parameters
#' @export
areaAttributes <- function(size = 0.5,
                           linetype = "solid",
                           color = NA,
                           fillColor = "red",
                           alpha = 0.25){
  return(list(size = size,
              linetype = linetype,
              color = color,
              fillColor = fillColor,
              alpha = alpha))
}

#' helper function to generate an area definition
#' 
#' @param x x-coordinates of the 'corners' of the area
#' @param y y-coordinates of the 'corners' of the area
#' 
#' returns a data.frame with columns x & y
#' 
#' @note areaBlockExample <- areaDefinition(x = c(5,10,10,5),
#'                                          y = c(10,10,20,20))
#' @export
areaDefinition <- function(x = NA, y = NA){
  if (identical(x,NA) | identical(y,NA)){
    return(NA)
  } else {
    return(data.frame(x = x, y = y))
  }
}

#' to add closed (eg colored) areas to a list of ggplot objects
#'
#' @param graphs list of ggplot-objects to which the areas have to be added
#'  Note: MUST be a list
#' @param areas list (!) of data.frames of x and y columns defining the area
#'  to be drawn in the ggplot objects. Example: 
#'  areaBlockExample <- data.frame(x = c(5,10,10,5), y = c(10,10,20,20))
#' @param areaAttributes list which defines the type of area to be used for
#'  the areas
#'
#' @note due to the graphs parameter you can use:
#'  list(ggplot objects) %>% graphAdjust() %>% areaMarks()
#'
#' @returns a list of ggplot objects
#'
#' @export
areaMarks <- function(graphs,
                      areas = areaDefinition(),
                      areaAttributes = areaAttributes()){
  if (!identical(areas,NA)){
    for (counter in 1:(length(graphs))){
      for (counter2 in (1:length(areas))){
        graphs[[counter]] <- graphs[[counter]] +
          ggplot2::geom_polygon(data = areas[[counter2]],
                                ggplot2::aes_string(x = "x",y = "y"),
                                size = areaAttributes$size,
                                linetype = areaAttributes$linetype,
                                color = areaAttributes$color,
                                fill = areaAttributes$fillColor,
                                alpha = areaAttributes$alpha)
      }
    }
  }
  return(graphs)
}

#' function to generate ggplot object (in form of a single item list) from a
#'  list of data.frames of data
#' 
#' @param tables list of data.frames from which to generate the graphs
#'  Note: MUST be a list
#' @param x character vector defining which column from the data.frames to use
#'  for the x-axis
#' @param y character vector defining which column from the data.frames to use
#'  for the y-axis
#' @param lineWidth defines the width of the lines in the graphs drawn from the
#'  x,y coordinates
#' @param xLimits defines the range x coordinates c(minimum, maximum),
#'  all coordinates outsude this range are not drawn
#' @param yPercentage defines if y-axis maximum should be set to 100 percent
#' @note due to the graphs parameter you can use:
#'  graphCurves(tables) %>% graphAdjust() %>% lineMarks() and similar pipes
#'
#' @returns a (single element) list of ggplot objects
#' @export
graphCurves <- function(tables, x, y, lineWidth = 0.5, xLimits = NA,
                        yPercentage = FALSE){
  tempList <- list()
  for (counter in 1:(length(tables))){
    if (!identical(xLimits,NA)){
      tables[[counter]] <- tables[[counter]] %>% dplyr::filter(!!dplyr::sym(x) >= xLimits[1] & !!dplyr::sym(x) <= xLimits[2])
    }
    if (yPercentage){
      tables[[counter]][y] <- (tables[[counter]][y]/max(tables[[counter]][y], na.rm = TRUE))*100
    }
    tempList[[counter]] <- tables[[counter]] %>%
      ggplot2::ggplot(ggplot2::aes_string(x,y)) + ggplot2::geom_line(size = lineWidth)
  }
  return(tempList)
}

#' to generate a ggplot object (actually a list(ggplot object) ) which combines
#' the data of a number of tables
#' 
#' @param tables list of data.frames from which to generate the graphs
#'  Note: MUST be a list
#' @param x character vector defining which column from the data.frames to use
#'  for the x-axis
#' @param y character vector defining which column from the data.frames to use
#'  for the y-axis
#' @param lineWidth defines the width of the lines in the graphs drawn from the
#'  x,y coordinates
#' @param xLimits defines the range x coordinates c(minimum, maximum),
#'  all coordinates outsude this range are not drawn
#' @param yPercentage defines if y-axis maximum should be set to 100 percent
#' @param combine if NA then all tables are combined into one, otherwise it
#'  has to be a integer vector defining which tables from the 'tables' list
#'  to combine eg c(1,2,4)
#' @param colors character vector describing the colors to be used. If a single
#'  color (default = "black") then all curves will be the same color, otherwise
#'  must be character vector of same length as the combine vector
#'
#' @returns a list of ggplot objects
#' @export
graphCurvesCombine <- function(tables, x, y, lineWidth = 0.5, xLimits = NA,
                               yPercentage = FALSE, combine = NA,
                               colors = "black"){
  tempList <- list()
  if (!identical(combine, NA)){
    tables <- tables[combine]
  }
  tempList[[1]] <- ggplot2::ggplot()
  for (counter in 1:(length(tables))){
    if (!identical(xLimits,NA)){
      tables[[counter]] <- tables[[counter]] %>% dplyr::filter(!!dplyr::sym(x) >= xLimits[1] & !!dplyr::sym(x) <= xLimits[2])
    }
    if (yPercentage){
      tables[[counter]][y] <- (tables[[counter]][y]/max(tables[[counter]][y], na.rm = TRUE))*100
    }
    tempList[[1]] <- tempList[[1]] + ggplot2::geom_line(data = tables[[counter]], ggplot2::aes_string(x,y),
                                               size = ifelse(length(lineWidth)>1,lineWidth[counter], lineWidth),
                                               color = ifelse(length(colors)>1,colors[counter], colors))
  }
  return(tempList)
}

#' helper function for the function customLegend():
#'  generates a data.frame specifying the legend to be generated
#' 
#' @param labels labels of the elements of the legend (character vector)
#' @param colors colors of the elements of the legend
#' @param fills fill colors of the elements of the legend
#' @param shapes shapes of the elements of the legend
#' @param sizes sizes of the elements of the legend
#' 
#' @returns a data.frame
#' @export
legendDefinition <-function(labels = NA, colors = NA,
                          fills = NA, shapes = NA,
                          sizes = NA){
  return(data.frame(labels = labels,
                    colors = colors,
                    fills = fills,
                    shapes = shapes,
                    sizes = sizes))
}

#' a function that takes a series of colors/fills/shapes and adds it as a
#'  legend to a ggplot object
#'  
#' @param p ggplot object to which the legend has to be added
#' @param legend each row of the legend data.frame is an item in the legend
#'  to be created, so the color, shape, etc of a single row 'belong' together
#'  The row-order determines the order in the legend
#' @param legend.title defines title of the legend
#' @param legend.title.face defines typography of the legend title
#' @param legend.title.size defines size of the legend title
#' @param legend.element.size defines size of the items in the legend
#' @param legend.position defines the placement of the legend
#' @param fakePosition defines the position of the data point of the fake
#'  dataset used to generate the legend (trick). Choose this to be (well)
#'  outside of the actual data in the ggplot object p. If NA (not defined), it
#'  will be set to (0,0) or (if axesAsis = TRUE) outside the current ranges of
#'  the axes
#' @param axesAsis if TRUE then the exes ranges are left as they were, ie the
#'  argument fakePoistion will have no influence on the axes ranges. Note that
#'  this uses the function ggplot_build() function which seems somewhat
#'  experimental, meaning it may work differently in future versions of ggplot2
#'  
#' @note use of this function will result in a series of warning messages
#'  (one for every item in the legend). This is 'normal' because the function
#'  uses a trick to do its thing. Use suprressWarnings(print()) to prevent
#'  seeing the messages while running code. In R markdown use the option
#'  'warning=FALSE' to not see the messages
#' @note it's advisable to make this customLegend the last thing to be added to
#'  the ggplot object p (make customLegend the last in the %>% / pipe series)
#'    
#' @returns a ggplot object
#' @export
customLegend <- function(p, legend = legendDefinition(),
                         legend.title = "Legend",
                         legend.title.face = "bold",
                         legend.title.size = 12,
                         legend.element.size = 12,
                         legend.position = "bottom",
                         fakePosition = NA,
                         axesAsis = TRUE){
  if (axesAsis){
    xLimits <- ggplot2::ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
    yLimits <- ggplot2::ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
  }
  if (identical(fakePosition,NA)){
    fakePosition <- c(0,0)
    if (axesAsis){
      if (xLimits[2] > xLimits[1]){
        fakePosition[1] <- xLimits[1] - abs(xLimits[2]-xLimits[1])
      } else {
        fakePosition[1] <- xLimits[2] - abs(xLimits[1]-xLimits[2])
      }
      if (yLimits[2] > yLimits[1]){
        fakePosition[2] <- yLimits[1] - abs(yLimits[2]-yLimits[1])
      } else {
        fakePosition[2] <- yLimits[2] - abs(yLimits[1]-yLimits[2])
      }
    } 
  }
  for (counter in 1:(nrow(legend))){
    fakedf <- data.frame(x1x = fakePosition[1],
                         y1y = fakePosition[2],
                         group = legend$labels[counter])
    p <- p + ggplot2::geom_point(data = fakedf, ggplot2::aes_string(x = "x1x", y = "y1y",
                                           color = "group",
                                           fill = "group",
                                           shape = "group",
                                           size = "group"))
  }
  p <- p +
    ggplot2::scale_color_manual(values = legend$colors, breaks = legend$labels) +
    ggplot2::scale_fill_manual(values = legend$fills, breaks = legend$labels) +
    ggplot2::scale_shape_manual(values = legend$shapes, breaks = legend$labels) +
    ggplot2::scale_size_manual(values = legend$sizes, breaks = legend$labels) +
    ggplot2::labs(color = legend.title, fill = legend.title, shape = legend.title, size = legend.title) +
    ggplot2::theme(legend.title=ggplot2::element_text(size = legend.title.size, face = legend.title.face),
          legend.margin=ggplot2::margin(l=0),
          legend.text=ggplot2::element_text(size = legend.element.size),
          legend.position = legend.position)
  if (axesAsis){
    p <- p + 
      ggplot2::scale_x_continuous(limits = xLimits) + 
      ggplot2::scale_y_continuous(limits = yLimits)
  }
  return(p)
}