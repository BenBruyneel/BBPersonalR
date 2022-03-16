library(dplyr)
library(tolerance)

#' returns a data.frame with the specified quantile limits of data
#' 
#' @param data the data to be used, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which column is to be used for calculating the
#'  quantiles. Can be integer or character (column name)
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to calculating the quantiles. @note this will remove warning messages and
#'  errors
#' @param limits a numeric vector specifying the quantile limits to be
#'  calculated. @note these values should be between 0 and 1
#' @param label defines the name of the calculated limits column. Under 'normal'
#'  circumstances this would be the column name of the data
#' @param quantilePercentage boolean defining whether the percentile column 
#'  output should percentages of the values specified in limits parameter
#'  
#' @returns a data.frame with two columns: quanitle and the limits calculated
#'  from the data
#' @export
tableQuantiles <- function(data, column, removeNA = TRUE,
                           limits = c(1,0.995, 0.975, 0.90, 0.75, 0.50,
                                      0.25, 0.10, 0.025, 0.5, 0.0),
                           label = NA, quantilePercentage = TRUE){
  if (is.Class(data, "data.frame")){
    if (length(column)>1){
      return(NA)
    }
    data <- data[column]
    if (identical(label, NA)){
      label <- colnames(data)
    }
  } else {
    if (identical(label,NA)){
      label <- "x"
    }
  }
  data = quantile(data, probs = limits, na.rm = removeNA)
  if (quantilePercentage){
    df <- data.frame(quantile = names(data), x2 = data,
                     stringsAsFactors = FALSE)
  } else {
    df <- data.frame(quantile = limits, x2 = data,
                     stringsAsFactors = FALSE)
  }
  colnames(df)[2] <- label
  return(df)
}

#' returns a statistics summary table of the specified data (mean and
#'  associated statistical measures)
#' 
#' @param data the data to be used, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which column is to be used for calculating the
#'  statistics. Can be integer or character (column name)
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to the calculations. @note this will remove warning messages and errors
#' @param alpha specifies the confidence level to be used. @note this value
#'  should be between 0 and 1
#'  
#' @returns a data.frame with two columns: statistic name and the calculated
#'  statistic itself
#' @export
tableSummary <- function(data, column, removeNA = TRUE, alpha = 0.05){
  if (is.Class(data, "data.frame")){
    if (length(column)>1){
      return(NA)
    }
    data <- data[,column]
  }
  df <- data.frame(statistic = c("Mean", "Standard Deviation",
                                 "Standard Error Mean",
                                 "Upper level mean", "Lower level mean",
                                 "Confidence Level (%)", "n","NA"),
                   value = c(0,0,0,0,0,0,0,0))
  df$value[1] <- mean(data, na.rm = removeNA)
  df$value[2] <- sd(data, na.rm = removeNA)
  if (removeNA){
    df$value[7] <- data %>% na.omit() %>% length()
  } else {
    df$value[7] <- data %>% length()
  }
  df$value[3] <- df$value[2]/sqrt(df$value[7])
  df$value[4] <- df$value[1] + (qt(1-(alpha/2),
                                   df = df$value[7]-1) * df$value[3])
  df$value[5] <- df$value[1] - (qt(1-(alpha/2),
                                   df = df$value[7]-1) * df$value[3]) 
  df$value[6] <- (1-alpha)*100
  df$value[8] <- sum(is.na(data))
  return(df)
}

#' returns a confidence interval table of the specified data (mean, standard 
#'  deviation and associated statistical measures)
#' 
#' @param data the data to be used, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which column is to be used for calculating the
#'  quantiles. Can be integer or character (column name)
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to calculating the quantiles. @note this will remove warning messages and
#'  errors
#' @param alpha (1-confidence level) for the upper and lower levels of the mean
#'  and the standard deviation
#'  
#' @returns a data.frame with two columns: statistic and the values calculated
#'  from the data
#' @export
confidenceIntervals <- function(data, column, removeNA = TRUE, alpha = 0.05){
  if (is.Class(data, "data.frame")){
    if (length(column)>1){
      return(NA)
    }
    data <- data[,column]
  }
  df <- data.frame(statistic = c("Mean",
                                 "Upper level mean", "Lower level mean",
                                 "std dev",
                                 "Upper level std dev", "Lower level std dev",
                                 "N", "Confidence Level"), 
                   value = c(0,0,0,0,0,0,0,0))
  if (removeNA){
    stdErrMean <-  sd(data, na.rm = removeNA)/sqrt(length(data %>% na.omit()))
  } else {
    stdErrMean <-  sd(data, na.rm = removeNA)/sqrt(length(data))
  }
  df$value[1] <- mean(data, na.rm = removeNA)
  df$value[4] <- sd(data, na.rm = removeNA)
  if (removeNA){
    df$value[7] <- data %>% na.omit() %>% length()
  } else {
    df$value[7] <- data %>% length()
  }
  df$value[8] <- (1-alpha)*100
  df$value[2] <- df$value[1] + (qt(1-(alpha/2),
                                   df = df$value[7]-1) * stdErrMean)
  df$value[3] <- df$value[1] - (qt(1-(alpha/2),
                                   df = df$value[7]-1) * stdErrMean)
  df$value[5] <- sqrt(((df$value[7]-1)*(df$value[4]^2))/
                        (qchisq((alpha/2), df = df$value[7]-1)))
  df$value[6] <- sqrt(((df$value[7]-1)*(df$value[4]^2))/
                        (qchisq(1-(alpha/2), df = df$value[7]-1)))
  return(df)
}

#' returns a data.frame with 2-sided tolerance intervals for the data
#'  distributed according to normal distribution
#' 
#' @param data the data to be used, can be a numeric/character/etc vector or
#'  data.frame like (or tibble etc). If it is data.frame or similar the column
#'  argument defines which column is to be used
#' @param column defines which column is to be used for calculating the
#'  quantiles. Can be integer or character (column name)
#' @param removeNA if TRUE, the NA 'values' in the vector will be removed prior
#'  to calculating the quantiles. @note this will remove warning messages and
#'  errors
#' @param alpha (1-confidence level), specifies the confidence level required
#' @param proportion specifies proportion of the population
#' 
#' @note tolerance interval = the range that covers a specified proportion of
#'  the population with a given level of confidence (1-alpha). It expresses the
#'  range where individual values from the population are expect to be
#'  
#' @returns a data.frame with two columns: statistic and the values calculated
#'  from the data
#' @export
toleranceInterval <- function(data, column, removeNA = TRUE,
                              alpha = 0.05, proportion = 0.95){
  if (is.Class(data, "data.frame")){
    if (length(column)>1){
      return(NA)
    }
    data <- data[,column]
  }
  if (removeNA){
    data <- data %>% na.omit()
  }
  ni <- normtol.int(data, alpha = alpha, P = proportion, side = 2)
  return(data.frame(statistic = c("confidence (alpha)","proportion",
                                  "Lower","Upper"),
                    value = c(alpha,proportion,
                              ni$`2-sided.lower`, ni$`2-sided.upper`)))
  
}