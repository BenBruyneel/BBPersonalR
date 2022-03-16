library(MASS)
library(dplyr)
library(tidyselect)
library(ggplot2)
library(flextable)

#' generates vector of weighted values
#' 
#' @param weight default = NULL, no weights, function will simply return NULL
#'  otherwsie it will return a weighted version of wvalues according to the
#'  formula:   1/wvalues^weight .  No other transformation of wvalues possible
#' @param wvalues a vector (numeric/integer/...) of values to be weighted
#' @param infiniteValue infinite values can give problems with some plot or
#'  calculations, default is set to NA. Set to NULL if no conversion is needed
#' 
#' @returns a vector of weighted values (or NULL) 
#' @export
weightsVector <- function(weight = NULL, wvalues, infiniteValue = NA){
  if (is.null(weight)){
    return(NULL)
  } else {
    tempVector <- abs(1/(wvalues^(as.integer(weight))))
    if (!identical(infiniteValue,NULL)){
      tempVector[is.infinite(tempVector)] <- infiniteValue
    }
    return(tempVector)
  }
}

#' generates vector of weighted values where the weights are calculated from
#'  the residuals coming from a fit
#' 
#' @param fit model object (eg coming from lm() or fitLM())
#' 
#' @returns a vector of weighted values (or NULL)
#' @export
weightsFromResidualsVector <- function(fit){
  return(1/stats::lm(abs(fit$residuals) ~ fit$fitted.values)$fitted.values^2)
}

#' wrapper around the lm() function to streamline it's use a bit
#' 
#' @param fitTable data.frame containing the data to be fitted
#' @param predictorColumn can be integer (column number) or character
#'  (column name)
#' @param responseColumn can be integer (column number) or character
#'  (column name)
#' @param weights default = NULL, otherwise must be integer to give weights to
#'  the predictor values (see also function weightsVector) or a numeric
#'  vector with length = number of rows in calibrationTable
#'  
#'  @returns fit object of type "lm"
#'  @export
fitLM <- function(fitTable,
                   predictorColumn = 1, responseColumn = 2,
                   weights = NULL){
  x <- fitTable[,predictorColumn]
  y <- fitTable[,responseColumn]
  if (length(weights) == 1){
      return(stats::lm(formula = y ~ x, weights = weightsVector(weight = weights,x)))
  } else {
    if (is.null(weights)){
      return(stats::lm(formula = y ~ x))
    } else {
      return(stats::lm(formula = y ~ x, weights = weights))
    }
    
  }
}

#' creates a fit table data.frame with the columns predictorColumn,
#'  ResponseColumn, Calculated Predictor, Recovery of predictor
#' 
#' @param fitTable data.frame containing the data to be fitted
#' @param predictorColumn can be integer (column number) or character
#'  (column name)
#' @param responseColumn can be integer (column number) or character
#'  (column name)
#' @param weights default = NULL, otherwise must be integer to give weights to
#'  the predictor values (see also function weightsVector) or a numeric
#'  vector with length = number of rows in calibrationTable
#' @param fit model object (eg coming from lm() or fitLM())
#' @param removeInf Due to the nature of the calculations, the recovery
#'  sometimes results in +Inf or -Inf (infinite values). If this option is TRUE
#'  then these values are replaced by NA
#' 
#' @returns a data.frame witg columns: predictorColumn, ResponseColumn,
#'  Calculated Predictor, Recovery of predictor 
#' @export
fitDF <- function(fitTable, predictorColumn = 1, responseColumn = 2, 
                  weights = NULL,
                  fit = fitLM(fitTable,
                               predictorColumn = predictorColumn,
                               responseColumn = responseColumn,
                               weights = weights),
                  removeInf = TRUE){
  if (length(weights) == 1) {
      weights <- weightsVector(weight = weights, fitTable[, predictorColumn])
  }
  if (!is.character(predictorColumn)){
    predictorColumn <- colnames(fitTable)[predictorColumn]
  }
  if (!is.character(responseColumn)){
    responseColumn <- colnames(fitTable)[responseColumn]
  }
  if (!is.null(weights)){
    fitTable$calculated <- unname(stats::predict(fit,
                                          newdata =
                                            {
                                              tempdf <- data.frame(x = fitTable[,predictorColumn])
                                              colnames(tempdf)[1] <- predictorColumn
                                              tempdf
                                            },
                                          weights = weights))
  } else {
    fitTable$calculated <- unname(stats::predict(fit,
                                          newdata = 
                                            {
                                              tempdf <- data.frame(x = fitTable[,predictorColumn])
                                              colnames(tempdf)[1] <- predictorColumn
                                              tempdf
                                            }))
  }
  fitTable$recovery <- (((fitTable$calculated - fitTable[, responseColumn])/fitTable[, responseColumn])*100)+100
  if (removeInf){
    fitTable$recovery[is.infinite(fitTable$recovery)] <- NA
  }
  return(fitTable)
}

#' support function: changes character vector of numbers into numerical vector
#'  (and adds NA's when appropriate)
#'  
#' @param chrs character vector of numbers in 'text' format
#' 
#' @returns numeric vector
#' @export
toNumeric <- function(chrs = "N/A"){
  return(unlist(lapply(chrs,
                       function(x){
                         if (identical(x,"N/A")){
                           NA
                         } else {
                           suppressWarnings(as.numeric(x))
                         }
                       }
  )
  )
  )
}

#' generates a ggplot object of the fit table
#'
#' @param fitTable data.frame containing the data to be fitted
#' @param predictorColumn can be integer (column number) or character
#'  (column name)
#' @param responseColumn can be integer (column number) or character
#'  (column name)
#' @param weights default = NULL, otherwise must be integer to give weights to
#'  the predictor values (see also function weightsVector) or a numeric
#'  vector with length = number of rows in calibrationTable. Note: if a vector
#'  of weights if provided, the prediction to the minimal/maximal values on the
#'  x- & y-axis is not possible
#' @param fit model object (eg coming from lm() or fitLM())
#' @param title title for the graph, character vector
#' @param figureNo number of the figure, used for the caption ("Figure ..." )
#' @param caption caption for the graph, character vector
#' @param autoScalePredictor if TRUE then default autoscaling of the x-axis
#'  takes place, most other settings delaing with scaling of the x-axis the are
#'  then ignored
#' @param autoScalePredictor if TRUE then default autoscaling of the y-axis
#'  takes place,most other settings delaing with scaling of the y-axis the are
#'  then ignored
#' @param predictorLimits 2 element numeric vector with the minimum and maximum value
#'  for the x-axis. If an element is a character vector or something that
#'  cannot be converted to a numeric value, then NA is used which leads to
#'  minimum/maximum value
#' @param responseLimits 2 element numeric vector with the minimum and maximum value
#'  for the y-axis. If an element is a character vector or something that
#'  cannot be converted to a numeric value, then NA is used which leads to
#'  minimum/maximum value
#' @param predictorLabel name of the x-axis, character vector
#' @param responseLabel name of the y-axis, character vector
#' @param predictorOob integer vector, indicating how to deal with out of bounds
#'  datapoints. 1 = censor scales::oob_censor is used, 2 = infinite,
#'  scales::oob_squish_infinite is used. Note: x-axis only
#' @param respomseOob integer vector, indicating how to deal with out of bounds
#'  datapoints. 1 = censor scales::oob_censor is used, 2 = infinite,
#'  scales::oob_squish_infinite is used. Note: y-axis only
#' @param regressionColor color of the regression line
#' @param regressionLineType appearance of the regression line: 0 = blank,
#'  1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
#' @param regressionWidth width of the regression line (default 1)
#' @param regressionAlpha alpha ('seethrough' value) of the regressionline
#' @param pointsColor (border) color of the datapoints
#' @param pointsFill fill color, note: only some shapes have a fill color
#' @param pointsShape shape of the datapoints (default = 21), see vignette
#'  ggplot2::ggplot2-specs
#' @param pointsSize size of the datapoints
#' @param pointsAlpha alpha ('seethrough' value) of the datapoints
#' @param showConfidence defines if confidence level needs to be shown
#'  (usually shown as a shaded area around the line)
#' @param confidenceLevel defines the confidence level (range 0-1)
#' @param confidenceColor color of the confidence 'area'
#' @param confidenceAlpha alpha ('seethrough' value) of the confidence 'area'
#' @param showPrediction defines if prediction level needs to be shown
#'  (usually shown as a shaded area around the line)
#' @param predictionLevel defines the prediction level (range 0-1)
#' @param predictionColor color of the prediction 'area'
#' @param predictionAlpha alpha ('seethrough' value) of the prediction 'area'
#' @param rotateGraph if TRUE, then x-axis and y-axis are swapped, essentially
#'  resulting in a graph rotated 90 degrees
#' @param predictorOptions x-axis options character vector, "1" normal scale,
#'  "2" reversed scale, "3" logarithmic scale (log10)
#' @param responseOptions y-axis options character vector, "1" normal scale,
#'  "2" reversed scale,"3" logarithmic scale (log10)
#'  
#' @returns ggplot object
#' @export
fitPlot <- function(fitTable,
                    predictorColumn = 1, responseColumn = 2, 
                    weights = NULL,
                    fit = fitLM(fitTable,
                                 predictorColumn = predictorColumn,
                                 responseColumn = responseColumn,
                                 weights = weights),
                    title = "", figureNo = 1, 
                    caption = paste(c("Figure ",
                                      figureNo,
                                      ": Calibration Curve"),
                                    collapse = ""),
                    autoScalePredictor = FALSE,
                    predictorLimits = c(0,"N/A"),
                    predictorLabel = NULL, predictorOob = 1,
                    autoScaleResponse = FALSE,
                    responseLimits = c(0,"N/A"),
                    responseLabel = NULL, responseOob = 1,
                    regressionColor = "red", regressionLineType = 1,
                    regressionWidth = 1, regressionAlpha = 1,
                    pointsColor = "Black", pointsFill = "black",
                    pointsShape = 21, pointsSize = 2, pointsAlpha = 1,
                    showConfidence = TRUE, confidenceLevel = 0.95,
                    confidenceColor = "green", confidenceAlpha = 0.2,
                    showPrediction = FALSE, predictionLevel = 0.95,
                    predictionColor = "blue", predictionAlpha = 0.05,
                    rotateGraph = FALSE,
                    predictorOptions = "1", responseOptions = "1"){ 
  #if not names of columns, put column names in predictorColumn & responseColumn
  if (!is.character(predictorColumn)){
    predictorColumn <- colnames(fitTable)[predictorColumn]
  }
  if (!is.character(responseColumn)){
    responseColumn <- colnames(fitTable)[responseColumn]
  }
  tempdf <- fitTable %>% dplyr::select(tidyselect::all_of(c(predictorColumn,
                                                responseColumn)))
  colnames(tempdf) <- c("x","y")
  g <- ggplot2::ggplot(data = tempdf,ggplot2::aes_string(x = "x", y = "y"))
    if (!autoScalePredictor){  # add numbers  to make lm hit the axis
      x2 <- c(toNumeric(predictorLimits[1]),fitTable[,predictorColumn],
              toNumeric(predictorLimits[2]))
      x2[is.na(x2)] <- max(fitTable[,predictorColumn], na.rm = TRUE)
    } else {
      x2 <- fitTable[,predictorColumn]
    }
    if (!is.null(weights)){
      if (length(weights) == 1){
        yC <- as.data.frame(stats::predict(fit,
                                    newdata = data.frame(x = x2),
                                    weights = weightsVector(weight = weights, x2),
                                    interval = "confidence",
                                    level = confidenceLevel))
        yP <- as.data.frame(stats::predict(fit,
                                    newdata = data.frame(x = x2),
                                    weights = weightsVector(weight = weights, x2),
                                    interval = "prediction",
                                    level = predictionLevel))
      } else {
        x2 <- x2[-c(1,length(x2))] # no weights for min, max
        yC <- as.data.frame(stats::predict(fit,
                                    newdata = data.frame(x = x2),
                                    weights = weights,
                                    interval = "confidence",
                                    level = confidenceLevel))
        yP <- as.data.frame(stats::predict(fit,
                                    newdata = data.frame(x = x2),
                                    weights = weights,
                                    interval = "prediction",
                                    level = predictionLevel))
      }
    } else {
      yC <- as.data.frame(stats::predict(fit,
                                  newdata = data.frame(x = x2),
                                  interval = "confidence",
                                  level = confidenceLevel))
      yP <- as.data.frame(stats::predict(fit,
                                  newdata = data.frame(x = x2),
                                  interval = "prediction",
                                  level = predictionLevel))
    }
    if (showPrediction){
      g <- g + ggplot2::geom_ribbon(data = data.frame(x = x2, y = yC$fit),
                           ggplot2::aes(ymin = yP$lwr, ymax = yP$upr),
                           fill = predictionColor, alpha = predictionAlpha)
    }
    if (showConfidence){
      g <- g + ggplot2::geom_ribbon(data = data.frame(x = x2, y = yC$fit),
                           ggplot2::aes(ymin = yC$lwr, ymax = yC$upr),
                           fill = confidenceColor, alpha = confidenceAlpha)
    }
    g <- g + ggplot2::geom_line(data = data.frame(x = x2 ,y=yC$fit), ggplot2::aes_string(x="x",y="y"),
                       color = regressionColor,
                       linetype = regressionLineType,
                       size = regressionWidth,
                       alpha = regressionAlpha)
  g <- g + ggplot2::geom_point(shape = pointsShape,
                      size = pointsSize,
                      col = pointsColor, fill = pointsFill,
                      alpha = pointsAlpha)
  if (is.null(predictorLabel)){
    predictorLabel <- predictorColumn
  }
  
  if (is.null(responseLabel)){
    responseLabel <- responseColumn
  }
  g <- g + ggplot2::ylab(responseLabel) + ggplot2::xlab(predictorLabel)
  if (!autoScalePredictor){
    g <- g + ggplot2::scale_x_continuous(expand = c(0,0), limits = toNumeric(predictorLimits),
                                oob = switch(predictorOob,
                                             "1" = scales::oob_censor,
                                             "2" = scales::oob_squish_infinite))
  }
  if (!autoScaleResponse){
    g <- g + ggplot2::scale_y_continuous(expand = c(0,0), limits = toNumeric(responseLimits),
                                oob = switch(responseOob,
                                             "1" = scales::oob_censor,
                                             "2" = scales::oob_squish_infinite))
  }
  g <- g + ggplot2::ggtitle(title)
  g <- g + ggplot2::labs(caption = caption)
  
  g <- g + ggplot2::theme_light()
  
  g <- g + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 13,
                                             face = "italic"))
  
  if (rotateGraph){
    g <- g + ggplot2::coord_flip()
  }
  g <- switch(predictorOptions,
              "1" = g,
              "2" = g + ggplot2::scale_x_reverse(),
              "3" = g + ggplot2::scale_x_log10())
  g <- switch(responseOptions,
              "1" = g,
              "2" = g + ggplot2::scale_y_reverse(),
              "3" = g + ggplot2::scale_y_log10())
  g
}

#' helperfunction for limit line attributes
#' 
#' @param yIntercept  numeric vector where to place limit lines on the y-axis
#' @note Other parameters are either one per line or one for all
#' @param color colors of the limit lines
#' @param linetype linetype (solid, dotted, etc) of the limit lines
#' @param size width of the limit lines
#' @param alpha  alpha ('see through' factor) of the limit lines
#' 
#' @returns data.frame of the attributes
#' 
#' @note if no limits are wanted/needed, set at least one parameter to NA
#' @export
limits <- function(yIntercept = c(0),
                   color = c("blue"),
                   linetype = c(1),
                   size = c(1),
                   alpha = c(0.5)){
  if (identical(yIntercept,NA) |
      identical(color,NA) |
      identical(linetype,NA) |
      identical(size,NA) |
      identical(linetype,NA)){
    return(NA)
  } else {
    return(data.frame(yIntercept = yIntercept,
                      color = color,
                      linetype = linetype,
                      size = size,
                      alpha = alpha))
  }
}

#' wrapper function for (default) limit line attributes
#' 
#' @returns data.frame of the attributes
#' @export
stdResidualLimits <- function(){
  return(limits(yIntercept = c(0,0.68,-0.68,1.96,-1.96),
                color = "blue",
                linetype = c(1,3,3,2,2),
                size = 1,
                alpha = 0.5))
}

#' generates a ggplot object of the residual plot of a calibration table
#'
#' @param fitTable data.frame containing the data to be fitted
#' @param predictorColumn can be integer (column number) or character
#'  (column name)
#' @param responseColumn can be integer (column number) or character
#'  (column name)
#' @param weights default = NULL, otherwise must be integer to give weights to
#'  the predictor values (see also function weightsVector) or a numeric
#'  vector with length = number of rows in calibrationTable. Note: if a vector
#'  of weights if provided, the prediction to the minimal/maximal values on the
#'  x- & y-axis is not possible
#' @param fit model object (eg coming from lm() or fitLM())
#' @param title title for the graph, character vector
#' @param figureNo number of the figure, used for the caption ("Figure ..." )
#' @param caption caption for the graph, character vector
#' @param standardized if TRUE: use standardized residuals
#' @param showLimits  if TRUE, draw the lines for the limits defined in limits
#' @param limits definition of the limits to draw, see stdLiits, zeroLimits for
#'  examples
#' @param fitColor defines color of loess-type 'fit' to the residual coordinates
#' @param fitLineType defines line type of the 'fit' to the residual coordinates
#' @param fitWidth defines line width of the 'fit' to the residual coordinates
#' @param fitAlpha defines alpha of the 'fit' to the residual coordinates
#' @param pointsColor (border) color of the datapoints
#' @param pointsFill fill color, note: only some shapes have a fill color
#' @param pointsShape shape of the datapoints (default = 21), see vignette
#'  ggplot2::ggplot2-specs
#' @param pointsSize size of the datapoints
#' @param pointsAlpha alpha ('seethrough' value) of the datapoints
#' @param autoScaleX if TRUE then default autoscaling of the x-axis takes place,
#'  most other settings delaing with scaling of the x-axis the are then ignored
#' @param autoScaleY if TRUE then default autoscaling of the y-axis takes place,
#'  most other settings delaing with scaling of the y-axis the are then ignored
#' @param xLimits 2 element numeric vector with the minimum and maximum value
#'  for the x-axis. If an element is a character vector or something that
#'  cannot be converted to a numeric value, then NA is used which leads to
#'  minimum/maximum value
#' @param yLimits 2 element numeric vector with the minimum and maximum value
#'  for the y-axis. If an element is a character vector or something that
#'  cannot be converted to a numeric value, then NA is used which leads to
#'  minimum/maximum value
#' @param xLabel name of the x-axis, character vector
#' @param yLabel name of the y-axis, character vector
#' @param xOob integer vector, indicating how to deal with out of bounds
#'  datapoints. 1 = censor scales::oob_censor is used, 2 = infinite,
#'  scales::oob_squish_infinite is used. Note: x-axis only
#' @param yOob integer vector, indicating how to deal with out of bounds
#'  datapoints. 1 = censor scales::oob_censor is used, 2 = infinite,
#'  scales::oob_squish_infinite is used. Note: y-axis only
#' @param usePredictor if TRUE then the predictor is used as x-axis
#' @param useYLabel if TRUE then the y-label is used
#' @param rotateGraph if TRUE, then x-axis and y-axis are swapped, essentially
#'  resulting in a graph rotated 90 degrees
#' @param xOptions x-axis options character vector, "1" normal scale,
#'  "2" reversed scale, "3" logarithmic scale (log10)
#' @param xOptions y-axis options character vector, "1" normal scale,
#'  "2" reversed scale,"3" logarithmic scale (log10)
#'  
#' @returns ggplot object
#' @export
residualPlot <- function(fitTable,
                         predictorColumn = 1, responseColumn = 2, 
                         weights = NULL,
                         fit = fitLM(fitTable,
                                      predictorColumn = predictorColumn,
                                      responseColumn = responseColumn,
                                      weights = weights),
                         title = "", figureNo = 2,
                         caption = paste(c("Figure ",
                                           figureNo,
                                           ": Residual Plot"),
                                         collapse = ""),
                         standardized = TRUE,
                         showLimits = TRUE, limits = stdResidualLimits(),
                         fitColor = "red", fitLineType = 1,
                         fitWidth = 1, fitAlpha = 1,
                         pointsColor = "Black", pointsFill = "black",
                         pointsShape = 21, pointsSize = 2, pointsAlpha = 1,
                         autoScaleX = TRUE, xLimits = c(0,"N/A"),
                         xLabel = NULL, xOob = 2,
                         autoScaleY = TRUE, yLimits = c(0,"N/A"),
                         yLabel = NULL, yOob = 2,
                         usePredictor = TRUE, useYLabel = TRUE,
                         rotateGraph = FALSE,
                         xOptions = "1", yOptions = "1"){
  if (standardized){
    resStd <- MASS::stdres(fit)
  } else {
    resStd <- fit$residuals
  }
  if (!usePredictor){
    l <- ggplot2::ggplot(data.frame(x=fit$model$y,y=resStd),ggplot2::aes_string("x","y"))
  } else {
    l <- ggplot2::ggplot(data.frame(x=fit$model$x,y=resStd),ggplot2::aes_string("x","y"))
  }
  l <- l + ggplot2::stat_smooth(geom = "line",
                       color = fitColor,
                       linetype = fitLineType,
                       size = fitWidth,
                       alpha = fitAlpha)
  l <- l + ggplot2::geom_point(shape = pointsShape,
                      size = pointsSize,
                      col = pointsColor, fill = pointsFill,
                      alpha = pointsAlpha)
  l <- l + ggplot2::ylab(ifelse(useYLabel,
                       ifelse(standardized,"Standardized Residuals","Residuals"),
                       "")
  )
  # if not names of columns, put column names in predictorColumn & responseColumn
  if (!is.character(predictorColumn)){
    prColumn <- colnames(fitTable)[predictorColumn]
  } else {
    prColumn <- predictorColumn
  }
  if (!is.character(responseColumn)){
    rsColumn <- colnames(fitTable)[responseColumn]
  } else {
    rsColumn <- responseColumn 
  }
  l <- l + ggplot2::xlab(ifelse(usePredictor,
                       prColumn,
                       rsColumn))
  l <- l + ggplot2::ggtitle(title)
  l <- l + ggplot2::labs(caption = caption)
  if (showLimits){
    if (!identical(limits,NA)){
      for (counter in 1:nrow(limits)){
        l <- l + ggplot2::geom_hline(yintercept = limits$yIntercept[counter],
                            color = limits$color[counter],
                            linetype = limits$linetype[counter],
                            size = limits$size[counter],
                            alpha = limits$alpha[counter])
      }
    }
  }
  if (!autoScaleX){
    l <- l + ggplot2::scale_x_continuous(expand = c(0,0), limits = toNumeric(xLimits), oob = switch(xOob,
                                                                                           "1" = scales::oob_censor,
                                                                                           "2" = scales::oob_squish_infinite))
  }
  if (!autoScaleY){
    l <- l + ggplot2::scale_y_continuous(expand = c(0,0), limits = toNumeric(yLimits), oob = switch(yOob,
                                                                                           "1" = scales::oob_censor,
                                                                                           "2" = scales::oob_squish_infinite))
  }
  l <- l + ggplot2::theme_classic()
  l <- l + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0, size = 13, face = "italic"))
  if (rotateGraph){
    l <- l + ggplot2::coord_flip()
  }
  l <- switch(xOptions,
              "1" = l,
              "2" = l + ggplot2::scale_x_reverse(),
              "3" = l + ggplot2::scale_x_log10())
  l <- switch(yOptions,
              "1" = l,
              "2" = l + ggplot2::scale_y_reverse(),
              "3" = l + ggplot2::scale_y_log10())
  l
}

#' helper function to provide default limits for recoveryPlots
#' 
#' @returns data.frame of the attributes
#' @export
stdRecoveryLimits <- function(){
  return(limits(yIntercept = c(100,75,125,50,150),
                color = "blue",
                linetype = c(1,3,3,2,2),
                size = 1,
                alpha = 0.5))
}

#' generates a ggplot object of the recovery plot of a calibration table
#'
#' @param fitTable data.frame containing the data to be fitted
#' @param predictorColumn can be integer (column number) or character
#'  (column name)
#' @param responseColumn can be integer (column number) or character
#'  (column name)
#' @param weights default = NULL, otherwise must be integer to give weights to
#'  the predictor values (see also function weightsVector) or a numeric
#'  vector with length = number of rows in calibrationTable. Note: if a vector
#'  of weights if provided, the prediction to the minimal/maximal values on the
#'  x- & y-axis is not possible
#' @param fit model object (eg coming from lm() or fitLM())
#' @param title title for the graph, character vector
#' @param figureNo number of the figure, used for the caption ("Figure ..." )
#' @param caption caption for the graph, character vector
#' @param standardized if TRUE: use standardized residuals
#' @param showLimits  if TRUE, draw the lines for the limits defined in limits
#' @param limits definition of the limits to draw, see stdRecoveryLimits for an
#'  example
#' @param fitColor defines color of loess-type 'fit' to the recovery coordinates
#' @param fitLineType defines line type of the 'fit' to the recovery coordinates
#' @param fitWidth defines line width of the 'fit' to the recovery coordinates
#' @param fitAlpha defines alpha of the 'fit' to the residual coordinates
#' @param pointsColor (border) color of the data points
#' @param pointsFill fill color, note: only some shapes have a fill color
#' @param pointsShape shape of the data points (default = 21), see vignette
#'  ggplot2::ggplot2-specs
#' @param pointsSize size of the data points
#' @param pointsAlpha alpha ('seethrough' value) of the data points
#' @param autoScaleX if TRUE then default autoscaling of the x-axis takes place,
#'  most other settings delaing with scaling of the x-axis the are then ignored
#' @param autoScaleY if TRUE then default autoscaling of the y-axis takes place,
#'  most other settings delaing with scaling of the y-axis the are then ignored
#' @param xLimits 2 element numeric vector with the minimum and maximum value
#'  for the x-axis. If an element is a character vector or something that
#'  cannot be converted to a numeric value, then NA is used which leads to
#'  minimum/maximum value
#' @param yLimits 2 element numeric vectorN with the minimum and maximum value
#'  for the y-axis. If an element is a character vector or something that
#'  cannot be converted to a numeric value, then NA is used which leads to
#'  minimum/maximum value
#' @param xLabel name of the x-axis, character vector
#' @param yLabel name of the y-axis, character vector
#' @param xOob integer vector, indicating how to deal with out of bounds
#'  datapoints. 1 = censor scales::oob_censor is used, 2 = infinite,
#'  scales::oob_squish_infinite is used. Note: x-axis only
#' @param yOob integer vector, indicating how to deal with out of bounds
#'  datapoints. 1 = censor scales::oob_censor is used, 2 = infinite,
#'  scales::oob_squish_infinite is used. Note: y-axis only
#' @param usePredictor if TRUE then the predictor is used as x-axis
#' @param useYLabel if TRUE then the y-label is used
#' @param rotateGraph if TRUE, then x-axis and y-axis are swapped, essentially
#'  resulting in a graph rotated 90 degrees
#' @param xOptions x-axis options character vector, "1" normal scale,
#'  "2" reversed scale, "3" logarithmic scale (log10)
#' @param xOptions y-axis options character vector, "1" normal scale,
#'  "2" reversed scale,"3" logarithmic scale (log10)
#'  
#' @returns ggplot object
#' @export
recoveryPlot <- function(fitTable,
                         predictorColumn = 1, responseColumn = 2, 
                         weights = NULL,
                         fit = fitLM(fitTable,
                                      predictorColumn = predictorColumn,
                                      responseColumn = responseColumn,
                                      weights = weights),
                         title = "", figureNo = 3,
                         caption = paste(c("Figure ",
                                           figureNo,
                                           ": Recovery Plot"),
                                         collapse = ""),
                         showLimits = TRUE, limits = stdRecoveryLimits(),
                         fitColor = "red", fitLineType = 1,
                         fitWidth = 1, fitAlpha = 1,
                         pointsColor = "Black", pointsFill = "black",
                         pointsShape = 21, pointsSize = 2, pointsAlpha = 1,
                         autoScaleX = TRUE, xLimits = c("N/A","N/A"),
                         xLabel = NULL, xOob = 2,
                         autoScaleY = TRUE, yLimits = c("N/A","N/A"),
                         yLabel = NULL, yOob = 2,
                         usePredictor = TRUE, useYLabel = TRUE,
                         rotateGraph = FALSE,
                         xOptions = "1", yOptions = "1"){
  fitTable <- fitDF(fitTable = fitTable,
                    predictorColumn = predictorColumn,
                    responseColumn = responseColumn,
                    fit = fit,
                    weights = weights)
  if (!usePredictor){
    l <- ggplot2::ggplot(data.frame(x=fitTable[,responseColumn],
                           y= fitTable$recovery),ggplot2::aes_string("x","y"))
  } else {
    l <- ggplot2::ggplot(data.frame(x=fitTable[,predictorColumn],
                           y=fitTable$recovery),ggplot2::aes_string("x","y"))
  }
  l <- l + ggplot2::stat_smooth(geom = "line",
                       color = fitColor,
                       linetype = fitLineType,
                       size = fitWidth,
                       alpha = fitAlpha)
  l <- l + ggplot2::geom_point(shape = pointsShape,
                      size = pointsSize,
                      col = pointsColor, fill = pointsFill,
                      alpha = pointsAlpha)
  # if not names of columns, put column names in
  # predictorColumn & responseColumn
  if (!is.character(predictorColumn)){
    prColumn <- colnames(fitTable)[predictorColumn]
  } else {
    prColumn <- predictorColumn
  }
  if (!is.character(responseColumn)){
    rsColumn <- colnames(fitTable)[responseColumn]
  } else {
    rsColumn <- responseColumn 
  }
  l <- l + ggplot2::xlab(ifelse(usePredictor,prColumn,rsColumn))
  l <- l + ggplot2::ylab(ifelse(useYLabel,"Recovery (%)",""))
  l <- l + ggplot2::ggtitle(title)
  l <- l + ggplot2::labs(caption = caption)
  if (showLimits){
    if (!identical(limits,NA)){
      for (counter in 1:nrow(limits)){
        l <- l + ggplot2::geom_hline(yintercept = limits$yIntercept[counter],
                            color = limits$color[counter],
                            linetype = limits$linetype[counter],
                            size = limits$size[counter],
                            alpha = limits$alpha[counter])
      }
    }
  }
  if (!autoScaleX){
    l <- l + ggplot2::scale_x_continuous(expand = c(0,0), limits = toNumeric(xLimits),
                                oob = switch(xOob,
                                             "1" = scales::oob_censor,
                                             "2" = scales::oob_squish_infinite))
  }
  if (!autoScaleY){
    l <- l + ggplot2::scale_y_continuous(expand = c(0,0), limits = toNumeric(yLimits),
                                oob = switch(yOob,
                                             "1" = scales::oob_censor,
                                             "2" = scales::oob_squish_infinite))
  }
  l <- l + ggplot2::theme_classic()
  l <- l + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0,
                                             size = 13, face = "italic"))
  
  if (rotateGraph){
    l <- l + ggplot2::coord_flip()
  }
  l <- switch(xOptions,
              "1" = l,
              "2" = l + ggplot2::scale_x_reverse(),
              "3" = l + ggplot2::scale_x_log10())
  l <- switch(yOptions,
              "1" = l,
              "2" = l + ggplot2::scale_y_reverse(),
              "3" = l + ggplot2::scale_y_log10())
  l
}

#' creates a data.frame with the columns name, predictor,
#'  calculated response, lower & upper levels for the prediction
#' 
#' @param fit lm class object containg the fit info
#' @param weight default = NULL, otherwise must be integer to give weights to
#'  the predictor values (see also function weightsVector)
#' @note use same weight as used for fit!
#' @param sampleTable data.frame containing the data to be used for prediction,
#'  must contain column with (sample-)names and a prediction/or column
#' @param predictionColumn can be integer (column number) or character
#'  (column name) indicates the predictor column
#' @param nameColumn can be integer (column number) or character (column name)
#' @param predictionLevel default is 0.95, used for the prediction
#' 
#' @returns a data.frame
#' @export
newDataDF <- function(fit, weight = NULL,
                      sampleTable, nameColumn = 1, predictionColumn = 2,
                      predictionLevel = 0.95){
  if (is.character(nameColumn)){
    nameColumn <- which(colnames(sampleTable) == nameColumn)
  }
  if (is.character(predictionColumn)){
    predictionColumn <- which(colnames(sampleTable) == predictionColumn)
  }
  if (!is.null(weight)){
    prdf <- stats::predict(fit,
                    newdata = 
                      {
                        tempdf = data.frame(x = sampleTable[,predictionColumn])
                        colnames(tempdf)[1] <- colnames(fit$model)[2]
                        tempdf
                      },
                    weights = weightsVector(weight = weight, wvalues = sampleTable[, predictionColumn]),
                    interval = c("prediction"), level = predictionLevel)
  } else {
    prdf <- stats::predict(fit,
                    newdata = 
                      {
                        tempdf = data.frame(x = sampleTable[,predictionColumn])
                        colnames(tempdf)[1] <- colnames(fit$model)[2]
                        tempdf
                      },
                    interval = c("prediction"), level = predictionLevel)
  }
  sampleTable <- sampleTable %>% dplyr::select(tidyselect::all_of(c(nameColumn, predictionColumn)))
  sampleTable$calculated <- as.data.frame(prdf)$fit
  sampleTable$Lower <- as.data.frame(prdf)$lwr
  sampleTable$Upper <- as.data.frame(prdf)$upr
  columnNames <- c("Name", "Predictor","Calculated", "Lower","Upper")
  return(sampleTable)
}