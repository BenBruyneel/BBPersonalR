#' helper function to determine if object == whichClass or a descendant
#'  of whichClass.
#'
#' @param object a data object of some class
#' @param whichClass character string: class name to be tested
#' @return TRUE or FALSE
#' @export
is.Class <- function(object, whichClass){
  return(whichClass %in% class(object))
}

#' ifelse replacement for properly returning all datatypes.
#'
#' @param logicValue variable or expression resulting in TRUE or FALSE,
#'  if missing or not logical then the function will return NULL.
#' @param ifTrue variable or expression to be returned when logicValue == TRUE
#' @param ifFalse variable or expression to be returned when logicValue == FALSE
#'
#' @returns depending on logicValue, ifTrue ir ifFalse.
#' @note not vectorized
#' @export
ifelseProper <- function(logicValue = NULL, ifTrue = NULL, ifFalse = NULL){
  if (missing(logicValue)){
    return(NULL)
  } else {
    if (!is.logical(logicValue)){
      return(NULL)
    } else {
      if (logicValue){
        return(ifTrue)
      } else {
        return(ifFalse)
      }
    }
  }
}

#' programming tool to convert a vector in memory to code for inclusion
#'  into programs.
#'
#' @param vectorData can be vector of class character, integer, logical,
#'  numeric, etc. Note that custom data types may not work properly.
#' @param includeParenthesis boolean parameter defining if 'c()' should be
#'  included around the character vector returned.
#' @param trim if NA (default) nothing is done, else should be value "both",
#'  "left" or "right". It will then cause all vectors to be white space trimmed
#'  on the specified site. See ?stringr::str_trim for more details. Note that
#'  trimming is done after formatting, so may undo settings like wide = x'
#' @param ... is used to transfer settings to the base format() function. See
#'  ?format for more info
#'
#' @returns a character vector.
#' @export
vectorToCode <- function(vectorData, includeParenthesis = TRUE, trim = NA, ...){
  return(
    ifelse(class(vectorData) == "character",
            paste(c(ifelse(includeParenthesis,
                           "c(",
                           ""),
                    "'",
                    ifelse(identical(trim,NA),
                           paste(format(vectorData, ...), collapse = "','"),
                           paste(stringr::str_trim(format(vectorData, ...),
                                                   side = trim),
                                 collapse = "','")),
                    "'",
                    ifelse(includeParenthesis,
                                   ")",
                                   "")),
                  collapse = ""),
            paste(c(ifelse(includeParenthesis,
                           "c(",
                           ""),
                    ifelse(identical(trim,NA),
                           paste(format(vectorData, ...), collapse = ","),
                           paste(stringr::str_trim(format(vectorData, ...),
                                          side = trim),
                                 collapse = ",")),
                    ifelse(includeParenthesis,
                           ")",
                           "")),
                  collapse = ""))
  )
}

#' programming tool to convert a data.frame in memory to a character vector
#'  representing code for inclusion into programs.
#'
#' @param dataframe data.frame to be converted to code. Can be with and without
#'  data.
#' @param differentLines parameter that allows inclusion of new line and tab
#'  'characters' in the produced character vector. The intended effect is not
#'   seen when printing 'normally', use cat() for this.
#' @param includeData default is TRUE, it will then include the data itself in
#'  the produced character string. If FALSE, then it will not.
#' @param includeRowNames default is TRUE, is only used when includeData = TRUE,
#'  it will then add an extra line of code with rownames(...) <- ...
#' @param trim if NA (default) nothing is done, else should be value "both",
#'  "left" or "right". It will then cause all vectors to be white space trimmed
#'  on the specified site. See ?stringr::str_trim for more details. Note that
#'  trimming is done after formatting, so may undo settings like wide = x'
#' @param ... is used to transfer settings to the base format() function. See
#'  ?format for more info. Note that these settings are applied to all
#'  vector types!
#'
#' @returns a character vector.
#' @export
dataFrameToCode <- function(dataframe,
                            differentLines = "\n\t",
                            includeData = TRUE,
                            includeRowNames = TRUE,
                            trim = NA,
                            ...){
  classes <- lapply(dataframe, class)
  if (!includeData){
    resultCode <- paste(
      c("data.frame(",
        paste(c(purrr::map2_chr(names(classes),
                                unname(classes),
                                ~paste(c(.x," = ",.y,"()"),collapse = ""))),
              collapse = paste(", ",differentLines, sep = "")),
        ")"),
      collapse = "")
  } else {
    resultCode <- "data.frame("
    for (counter in 1:ncol(dataframe)){
      resultCode <- paste(c(resultCode,
                            colnames(dataframe)[counter],
                            " = c(",
                            vectorToCode(dataframe[,counter],
                                         includeParenthesis = FALSE,
                                         trim = trim,
                                         ...),
                            ifelse(counter != ncol(dataframe),
                                   paste("), ", differentLines, sep = ""),
                                   ")"))
                          ,collapse = "")
    }
    resultCode <- paste(c(resultCode,")"), collapse = "")
    if (includeRowNames){
      resultCode <- paste(c(resultCode,"\nrownames(...) <- ",
                            vectorToCode(rownames(dataframe), trim = "both")),
                          collapse = "" )
    }
  }
  return(resultCode)
}

#' Translates a character vector compromised "F" and "T" into a logical vector
#'  of FALSE and TRUE. All "F"'s become FALSE's, all "T"'s become TRUE'S
#'
#' @param characters character vector to be translated
#'
#' @return logical vector
#'
#' @note NA and NAN give unpredictable results
#' @export
characterToBoolean <- function(characters){
  return(as.logical(unlist(strsplit(characters, split =""))))
}

#' Translates a logical vector into a character vector compromised of "F" and
#'  "T". All TRUE's become "T"'s, all FALSE's become "F"'s
#'
#' @param booleans logical vector to be translated
#'
#' @return character vector
#'
#' @note NA and NAN give unpredictable results
#' @export
booleanToCharacter <- function(booleans){
  paste(purrr::map_chr(booleans, ~substr(as.character(.x), start = 1, stop = 1)), collapse = "")
}

#' takes an integer and converts it to a date vector.
#'  Orginally meant to convert integer date format as used by eg SQLite to R's
#'  Date format
#'
#' @param aDate days since origin, integer numbers to be converted Date format
#' @param origin character vector specifying the origin date
#'
#' @returns converted dates in Date format
#' @export 
integerToDate <- function(aDate, origin = "1970-01-01"){
  return(as.Date(aDate, origin = origin))
}

#' converts date format to integer numbers
#'
#' @param aDate Date formatted dates to be converted integer
#' @param origin date vector specifying the origin date
#'
#' @returns converted dates in integer format
#' @export
dateToInteger <- function(aDate, origin = as.Date("1970-01-01")){
  return(as.integer(julian(aDate, origin = origin)))
}

#' takes a 'Date' object or an integer and converts it to a character vector of
#'  a specified format. Also changes NA's into a specified alternative
#'
#' @param aDate date vector or integer vector (days since origin)
#' @param formatString defines the formatting of the date vector, internally the
#'  base::as.Date function is used (via integerToDate)
#' @param origin character vector specifying the origin date (only used if aDate
#'  argument is an integer)
#' @param na.alternative specifies what to use for NA elements in the aDate
#'  argument
#'
#' transformData(transformationFormula = "transformToDate(data, na.alternative = '-')")
#' transformToDate(c(Sys.Date(),NA, Sys.Date()+1))
#' transformToDate(c(Sys.Date(),NA, Sys.Date()+1), na.alternative = "-")
#' 
#'
#' @return character vector
#' @export
transformToDateString <- function(aDate, formatString = "%d/%m/%Y",
                                  origin = "1970-01-01",
                                  na.alternative = NA){
  if (is.na(na.alternative)){
    return(format(integerToDate(aDate, origin = origin), formatString))
  } else {
    return(tidyr::replace_na(format(integerToDate(aDate, origin = origin),
                                    formatString),
                             na.alternative))
  }
}


#' Replaces all NA's in data (data.frame, list, etc) with an alternative
#'
#' @param data data.frame, list or ... that contains NA's which need to be
#'  replaced
#' @param na.alternative the object with which NA's need to be replace. Be aware
#'  that a list or column of a data.frame may change class when taking this
#'  action!
#'
#' @return data with replaced NA's
#' @export
transformNA <- function(data, na.alternative = "-"){
  data[is.na(data)] <- na.alternative
  return(data)
}

#' takes logical data and replaces it with an alternative, eg "Yes" and "No"
#'
#' @param data data to be transformed, should be logicalm but can be 1's and 0's
#'  (internally these will be transformed into TRUE's and FALSE's respectively)
#' @param alternative specifies what to transform TRUE (element 2) or FALSE
#'  (element 1) into
#' @param na.alternative specifies what to use if NA is encountered
#'
#'  transform01Logical(c(T,F,F,T))
#'  transform01Logical(c(1,0,0,1))
#'  
#' @return data with alternatives in stead of TRUE/FALSE
#' @export
transform01Logical <- function(data, alternative = c("No","Yes"), na.alternative = " "){
  data <- as.logical(data) # simple type casting, meant for 0,1
  if (!identical(alternative, NA)){ # first of alternative is meant for 0/FALSE, second is meant for 1/TRUE 
    data <- alternative[data+1]
  } 
  if (!is.na(na.alternative)){
    data[is.na(data)] <- na.alternative
  }
  return(data)
}

#' joins together a named list of data.frame's with exact same columns. Adds
#'  a column to each data.frame in the list with the name of the list element
#'
#' @param listData the named list of data.frame's
#'
#' @return a data.frame
joinNamedDFList <- function(listData){
  dplyr::bind_rows(purrr::map2(listData, names(listData), ~bind_cols(data.frame(node = .y), .x)))
}

#' A which for multidimensional arrays
#'
#' @param A Array of booleans 
#'
#' @note Mark van der Loo 16.09.2011
#' @note https://www.r-bloggers.com/2011/09/a-multidimensional-which-function/
#' 
#' example code
#' set.seed(83)
#' ar3 <- array(rbinom(20,1,0.5), dim = c(4,5))
#' ar3
#' 
#' ar3[1,1] <- 2
#' ar3[2,1] <- 2
#' ar3[1,2] <- 2
#' ar3[3,4] <- 2
#' ar3[2,5] <- 2
#' ar3[4,5] <- 2
#' 
#' ar3
#' 
#' which(ar3 == 2)
#' multi.which(ar3 == 2)
#'
#' @return sum(A) x length(dim(A)) array of multi-indices where A == TRUE
#' @export 
multi.which <- function(A){
  if ( is.vector(A) ) return(which(A))
  d <- dim(A)
  T <- which(A) - 1
  nd <- length(d)
  t( sapply(T, function(t){
    I <- integer(nd)
    I[1] <- t %% d[1]
    sapply(2:nd, function(j){
      I[j] <<- (t %/% prod(d[1:(j-1)])) %% d[j]
    })
    I
  }) + 1 )
}

#' can determine the array-'position' of an element number
#' 
#' @param element integer coming usually coming from which(...). Note: can only
#'  be a single number
#' @param dimensions dimensions of the (multidimensional) array from which
#'  the element comes
#' 
#' @note based on multi.which by M vd Loo, 2011
#' @note https://www.r-bloggers.com/2011/09/a-multidimensional-which-function/
#' 
#' example code:
#' set.seed(83)
#' ar3 <- array(rbinom(20,1,0.5), dim = c(4,5))
#' ar3
#' 
#' ar3[1,1] <- 2
#' ar3[2,1] <- 2
#' ar3[1,2] <- 2
#' ar3[3,4] <- 2
#' ar3[2,5] <- 2
#' ar3[4,5] <- 2
#' 
#' ar3
#' 
#' which(ar3 == 2)
#' multi.which(ar3 == 2)
#' whichDim(20, dimensions = dim(ar3))
#' # to get positions of all elements that are 2
#' purrr::map_df(which(ar3 == 2), ~as.data.frame(whichDim(.x, dimensions = dim(ar3))))

#' 
#' 
#' @return array with position of the element
#' 
#' @export
whichDim <- function(element, dimensions){
  element <- element - 1
  nd <- length(dimensions)
  I <- integer(nd)
  I[1] <- element %% dimensions[1]
  sapply(2:nd, function(j){
    I[j] <<- (element %/% prod(dimensions[1:(j-1)])) %% dimensions[j]
  })
  return(matrix(I, nrow = 1) + 1)
}

