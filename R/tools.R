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

#' joins together a named list of data.frame's with exact same columns. Adds
#'  a column to each data.frame in the list with the name of the list element
#'
#' @param listData the named list of data.frame's
#'
#' @return a data.frame
joinNamedDFList <- function(listData){
  dplyr::bind_rows(purrr::map2(listData, names(listData), ~bind_cols(data.frame(node = .y), .x)))
}
