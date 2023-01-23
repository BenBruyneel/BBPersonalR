#' Converts a numeric, integer or character vector (multiple elements) to a
#'  single element character vector (a string)
#'
#' @param dataVector the data to be converted: integer, numeric, character or
#'  logical. Other data types have not been tested
#' @param collapseChar the character to be used as seperator in the end result.
#'  Note: this seperator should not be present in the dataVector (especially
#'  after transformation) as it will give problems when using the DBToVector
#'  function.
#' @param formatNumbers boolean specifying whether the dataVector should be
#'  formatted using the base::format function
#' @param removeNA if TRUE then all NA values are removed before the result is
#'  transformed into a single element character vector
#' @param ... further arguments to be passed onto to base::format, see
#'  ?base::format for more information. Ignored when formatNumbers == FALSE
#'
#' @returns a single element character vector
#' @export
vectorToDB <- function(dataVector = NA, collapseChar = ";",
                       formatNumbers = FALSE, removeNA = FALSE, ...){
  if (removeNA){
    dataVector <- dataVector[!is.na(dataVector)]
  }
  if (!formatNumbers){
    return(paste(dataVector,collapse = collapseChar))
  } else {
    return(paste(format(dataVector, ...),
                 collapse = collapseChar))
  }
}

#' does the reverse of the vectorToDB function: splits a single element
#'  character vector into seperate elements
#'
#' @param dbData single element character vector to be splitted into seperate
#'   elements
#' @param collapseChar seperator to split dbData into seperate elements
#' @param vectorClass class of the seperate elements, default = "integer".
#'   Other tested options are "numeric" and "character".
#'
#' @returns a vector of type vectorClass
#' @export
DBtoVector <- function(dbData = NA, collapseChar = ";", vectorClass = "integer"){
  if (identical(dbData,NA)){
    return(vector(mode = vectorClass, length = 0))
  } else {
    dataVector <- unlist(strsplit(dbData, split = collapseChar))
    dataVector <- unlist(
      lapply(dataVector, function(x){ ifelse(identical(x,"NA") | identical(x,"NaN"),
                                             as.vector(NA, mode = vectorClass),
                                             as.vector(x, mode = vectorClass))}))
    return(dataVector)
  }
}

#' Converts a numeric, integer or character vector (multiple elements) to a
#'  sort of raw vector. (actually a string representation of a raw vector).
#'  Note: also compresses data if required.
#'
#' @param dataVector the data to be converted: integer, numeric or character.
#'  Other data types have not been tested
#' @param collapseChar the character to be used as seperator in the end result.
#'  Note: this seperator should not be present in the dataVector (especially
#'  after transformation) as it will give problems when using the DBToVector
#'  function.
#' @param formatNumbers boolean specifying whether the dataVector should be
#'  formatted using the base::format function
#' @param type character string, the type of compression. See ?memCompress for
#'  details
#' @param ... further arguments to be passed onto to base::format, see
#'  ?base::format for more information. Ignored when formatNumbers == FALSE
#'
#' @returns a character vector
#' @export
vectorToBlob <- function(dataVector = NA, collapseChar = ";",
                         formatNumbers = FALSE, type = "gzip", ...){
  tempRaw <- memCompress(vectorToDB(dataVector = dataVector,
                                    collapseChar = collapseChar,
                                    formatNumbers = formatNumbers,
                                    ...),
                         type = type)
  tempRaw <- paste(sf::rawToHex(tempRaw), collapse = "")
  return(tempRaw)
}

#' does the reverse of the vectorToBlob function: splits a character/raw vector
#'  into seperate elements (after decompressing)
#'
#' @param blobData a character/raw vector to be splitted into seperate elements
#' @param collapseChar seperator to split dbData into seperate elements
#' @param vectorClass class of the seperate elements, default = "integer".
#'   Other tested options are "numeric" and "character".
#' @param type character string, the type of compression. See ?memCompress for
#'  details
#'
#' @returns a vector of type vectorClass
#' @export
blobToVector <- function(blobData = NA,
                         collapseChar = ";",
                         vectorClass = "integer",
                         type = "gzip"){
  blobData <- stringr::str_split(blobData,"")[[1]]
  blobData <- paste0(blobData[c(TRUE, FALSE)], blobData[c(FALSE, TRUE)])
  blobData <- wkb::hex2raw(blobData)
  return(DBtoVector(dbData = memDecompress(blobData,
                                           type = type,
                                           asChar = TRUE),
                    collapseChar = collapseChar,
                    vectorClass = vectorClass))
}

#' Converts data.frames to a single data.frame where each data.frame from the
#'  list is a row of blobs or character separated strings
#'
#' @param dfData the data to be converted: a list of data.frames; they can be
#'  different number of rows, but columns specified in columnNames must be
#'  present
#' @param columnNames character vector specifying which columns from the
#'  data.frame(s) to convert
#' @param saveClasses default FALSE. If TRUE then the first row of the resulting
#'  data.frame will be the class name (as a character vector)
#' @param collapseChar the character to be used as seperator in the end result.
#'  Note: this seperator should not be present in the dataVector (especially
#'  after transformation) as it will give problems when using the DBToVector
#'  function.
#' @param formatNumbers boolean specifying whether the dataVector should be
#'  formatted using the base::format function
#' @param toBlob should the data be converted to a raw vector or not
#' @param type character string, the type of compression. See ?memCompress for
#'  details
#' @param ... further arguments to be passed onto to base::format, see
#'  ?base::format for more information. Ignored when formatNumbers == FALSE
#'
#' @returns a data.frame
#' @export
convertDFtoDB <- function(dfData, columnNames = colnames(dfData[[1]]),
                          saveClasses = FALSE,
                          collapseChar = ";", formatNumbers = FALSE,
                          toBlob = TRUE, type = "gzip",
                          ...){
  df <- data.frame()
  for (counter in 1:(length(dfData))){
    df2 <- data.frame(x=1)
    df2$x[1] <- ifelseProper(toBlob,
                             vectorToBlob(
                               dataVector = dfData[[counter]][,1],
                               collapseChar = collapseChar,
                               formatNumbers = formatNumbers,
                               type = type, ...),
                             vectorToDB(
                               dataVector = dfData[[counter]][,1],
                               collapseChar = collapseChar,
                               formatNumbers = formatNumbers,
                               ...)
    )
    colnames(df2) = columnNames[1]
    if (length(columnNames) > 1){
      for (counter2 in 2:length(columnNames)){
        newDF <- data.frame(x = 1)
        newDF$x[1] <- ifelseProper(toBlob,
                                   vectorToBlob(
                                     dataVector = dfData[[counter]][,counter2],
                                     collapseChar = collapseChar,
                                     formatNumbers = formatNumbers,
                                     type = type, ...),
                                   vectorToDB(
                                     dataVector = dfData[[counter]][,counter2],
                                     collapseChar = collapseChar,
                                     formatNumbers = formatNumbers,
                                     ...)
        )
        colnames(newDF) <- columnNames[counter2]
        df2 <- dplyr::bind_cols(df2,newDF)
      }
    }
    df <- dplyr::bind_rows(df, df2)
  }
  if (saveClasses){
    tempClasses <- unlist(lapply(dfData[[1]], class))
    df <- dplyr::bind_rows(tempClasses, df)
  }
  return(df)
}

#' Does the reverse of convertDFtoDB: converts a data.frame of converted
#'  data back to its original data.frames. Each row from the provided data.frame
#'  (df) is converted to a separate data.frame
#'
#' @param df data.frame to be converted
#' @param columnNames character vector specifying which columns from the df
#'  argument are to be put in the converted data.frame(s)
#' @param restoreClasses default is FALSE. IF TRUE then the first row of the
#'  data.frame argument (df) must contain the classes (as character vectors) to
#'  be used for the columns
#' @param collapseChar the character to be used as seperator
#' @param vectorClass class of the seperate elements, default = "integer".
#'  Other tested options are "numeric" and "character". Note: all columns are
#'  the same class when vectorClass is a single element vector. The argument
#'  can be different types, but then all column types need to be specified, eg
#'  c("numeric", "integer")
#' @param fromBlob is the data raw datatype or not
#' @param type character string, the type of compression. See ?memCompress for
#'  details
#'
#' @returns a data.frame
#' @export
convertDBtoDF <- function(df, columnNames = colnames(df),
                          restoreClasses = FALSE,
                          collapseChar = ";",
                          vectorClass = "integer",
                          fromBlob = TRUE, type = "gzip"){
  if (identical(columnNames,NA)){
    stop("Error : no column names defined")
  }
  if (!restoreClasses){
    if (length(vectorClass) == 1){
      vectorClass <- rep(vectorClass, length(columnNames))
    } else {
      if (length(vectorClass) != length(columnNames)){
        stop("Error : length vectorClass argument != length columnNames")
      }
    }
  } else {
    vectorClass <- unname(unlist((df[1,])))
    df <- df %>% dplyr::slice(-1)
  }
  dfData <- list()
  for (counter in 1:(nrow(df))){
    dfData[[counter]] <- data.frame(
      ifelseProper(
        fromBlob,
        blobToVector(blobData = unlist(as.data.frame(df)[counter,
                                                         columnNames[1]]),
                     collapseChar = collapseChar,
                     vectorClass = vectorClass[[1]],
                     type = "gzip"),
        DBtoVector(dbData = as.data.frame(df)[counter,columnNames[1]],
                   collapseChar = collapseChar[[1]],
                   vectorClass = vectorClass[[1]])
      )
    )
    if (length(columnNames)>1){
      for (counter2 in 2:(length(columnNames))){
        suppressMessages(
          dfData[[counter]] <- dplyr::bind_cols(dfData[[counter]],
                                                data.frame(
                                                  ifelse(fromBlob,
                                                         data.frame(
                                                           blobToVector(blobData = unlist(as.data.frame(df)[counter,
                                                                                                            columnNames[counter2]]),
                                                                        collapseChar = collapseChar,
                                                                        vectorClass = vectorClass[[counter2]],
                                                                        type = "gzip")
                                                         ),
                                                         data.frame(
                                                           DBtoVector(dbData = as.data.frame(df)[counter,columnNames[counter2]],
                                                                      collapseChar = collapseChar,
                                                                      vectorClass = vectorClass[[counter2]])
                                                         )
                                                  )
                                                )
          )
        )
      }
    }
    colnames(dfData[[counter]]) <- columnNames
  }
  return(dfData)
}

#' Wrapper around pool::dbPool(): opens a database
#'
#' @param  fileName  a character vector specifying the name and location
#'                   of the database
#' @param  drv defines database connection type, default = RSQLite::SQLite()
#' @param  ... to pass on additional parameters to pool::dbPool, exmples are
#'             host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com"
#'             username = "guest"
#'             password = "guest"
#'
#' @return database access 'handle'
#' @note if no file with the name 'fileName' exists, then it will be created
#' (but obviously it will be empty, so most further commands will fail)
#' @note if fileName == ":memory:" the database will be an in-memory database
#' @export
db_open <- function(fileName, drv = RSQLite::SQLite(), ...){
  return(pool::dbPool(drv = drv,
                      dbname = fileName,
                      ...))
}

#' Wrapper around pool::pooClose():  closes an open database
#' (normally opened earlier via eg db_open())
#'
#'
#' @param db            database access 'handle' to be closed
#' @param suppressErrors logical (default = FALSE). For the somewhat unique
#'  situation that a database is possibly already closed. This happens eg when
#'  working with 'in memory' databases as is possible with SQLite; the closing
#'  of this type of database may already have happened when for instance a shiny
#'  application closes/shuts down
#'
#' @export
db_close <- function(db, suppressErrors = FALSE){
  if (suppressErrors){
   try(pool::poolClose(db), silent = TRUE)
  } else {
    pool::poolClose(db)
  }
}

#' find the number of rows in a table of an (open) database
#'
#' @param db        database access 'handle'
#' @param tableName name of the table
#' @return number of rows in the table
#'
#' @export
db_nrow <- function(db,tableName){
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  result <- (pool::dbGetQuery(conn,paste(c("SELECT COUNT(*) FROM ",tableName),
                                       collapse = ""))[1,1])
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  return(result)
}


#' find the number of columns in a table of an (open) database
#'
#' @param db        database access 'handle'
#' @param tableName name of the table
#' @return number of columns in the table
#'
#' @export
db_ncol <- function(db, tableName){
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  result <- length(pool::dbListFields(conn,tableName))
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  return(result)
}

#' get the classes of the different columns in a database table
#'
#' @param db        database access 'handle'
#' @param tableName name of the table
#' @return a data.frame with columns name & type (= class)
#'
#' @export
db_columnInfo <- function(db, tableName){
  # get first row of the table to determine names & classes
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  temp <- pool::dbGetQuery(conn,paste(c("SELECT * FROM ", tableName," LIMIT 1"), collapse = ""))
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  # determine classes, note that in case of class lists
  # (as with type OOP descendants) this statement only take the first name
  # of the list
  temp <- purrr::map_chr(temp,~class(.x)[1])
  return(data.frame(name = names(temp), type = unname(temp)))
}

#' gathers info on all tables in the database
#'
#' @param db    database access 'handle'
#' @return a data.frame with the following columns: name, nrows, info
#' every entry in the column info contains a data.frame with the names
#' of the columns in the table and their type (see also db_columnInfo)
#' @export
db_tbl_def <- function(db){
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  datazz <- pool::dbListTables(db)
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  dftbl <- data.frame(name = datazz,
                      nrows = unlist(lapply(datazz,
                                            function(x){db_nrow(db,x)})),
                      info = NA)
  for (counter in 1: length(datazz)){
    dftbl$info[counter] <- list(db_columnInfo(db,datazz[counter]))
  }
  return(dftbl)
}

#' 'global' variable: default primary key name
#'
#' @note in future: remove from export 
#' 
#' @export
defaultPrimaryKeyName <- "id_"


#' returns the default primary key name
#'
#' @return character vector
#' @export
defaultPrimaryKey <- function(){
  return("id_")
}

#' write a data to a table in an (open) database
#'
#' @param db              database access 'handle'
#' @param tableName       name of the table
#' @param theTable        data.frame containing data
#' @param primaryKeyName  name of the primary key if used (default = "id_")
#' @param append          boolean if TRUE (default) the data is appended to the
#'                        existing table
#' @param overwrite       boolean if TRUE, the data overwrites the existing
#'                        table (default = FALSE)
#' @param doNotChangePool boolean that sets whether the pool has to be locked/
#'  released inside this function. Usually this needs to be done, so default
#'  is FALSE. Set to TRUE if this locking/release is done outside of the
#'  function. Note that there is a sort of bug when working with the
#'  pool::dbWriteTable : when a sql constraint like UNIQUE is defined for the
#'  table used, this may result in a locked database upon error. It's advisable
#'  to set doNotChangePool to TRUE to prevent this from happening
#' @param row.names       by default FALSE, defines if row names of the theTable
#'                        should be included or not
#'
#' @return  nothing
#' @note    both append & overwrite will generate errors if used wrongly
#' @note    if colnum names of the table do not match the column names of the
#'          data.frame, then execution will stop
#' @note    data.frame columns of type list will not write properly to
#'          eg MySQL, use function ... in stead of this one. SQLite works
#'          without problems
#' @note    the overwrite = TRUE option has been found to 'destroy' foreign key
#'          constraints. This is probably because a table is redefined/
#'          reinitialized when a table is 'overwritten'. Use with care
#'
#' @export
db_writeTable <- function(db, tableName, theTable,
                          primaryKeyName = defaultPrimaryKey(),
                          overwrite = FALSE, append = !overwrite,
                          doNotChangePool = FALSE, row.names = FALSE){
  compareColNames <- pool::dbListFields(db, tableName)
  if (!is.na(primaryKeyName) & primaryKeyName != ""){
    compareColNames <- compareColNames[compareColNames != primaryKeyName]
  }
  if (identical(compareColNames %>% sort(), colnames(theTable) %>% sort())){
    if (!doNotChangePool){
      conn <- pool::poolCheckout(db)
      pool::dbBegin(conn)
      pool::dbWriteTable(conn,name = tableName, theTable,
                         append = append, overwrite = overwrite,
                         row.names = row.names)
      pool::dbCommit(conn = conn)
      pool::poolReturn(conn)
    } else {
      pool::dbWriteTable(db,name = tableName, theTable,
                         append = append, overwrite = overwrite,
                         row.names = row.names)
    }
  } else {
    stop("Error: column names not identical to column names in database")
  }
  invisible()
}

#' create a table in a database and (optionally) put data in it
#'
#' @param db                database access 'handle'
#' @param tableName         name of the table
#' @param columnDefinitions vector of character strings with each string of the
#'                          format "columnName columnType", eg
#'                          c("filename TEXT","fileversion TEXT")
#' @param foreignKeys       data.frame with columns idName, referenceTable,
#'                          referencePrimaryKey. This will give per row this
#'                          SQL synthax FOREIGN KEY (idName) REFERENCES
#'                          referenceTable(referencePrimaryKey). If set to
#'                          NA (default) then no foreign keys are added
#' @param addPrimary        add a autoincremented primary key (default = TRUE)
#' @param uniqueConstraints allows the setting of (SQL) UNIQUE constraints of
#'                          the form UNIQUE(..., ...). If NA, no constraints are
#'                          set, otherwise it should be a character vector
#'                          specifying the column names to be set as unique
#' @param primaryKeyName    name of the primary key if used (default = "id_")
#'  note: if this argument is the name of an existing column, the primary key
#'  will not be added, but the column will be set to 'primary key'. The
#'  primary key will not be set to autoincrement in this case!
#' @param dataframe         data.frame containing data to be written to the
#'                          newly created database
#' @param dbType            default is "SQLite", only one alternative ("MySQL")
#'                          has been tested
#'
#' @return nothing
#' @note this may seem somewhat cumbersome way to create a table, when compared
#' to dplyr::copy_to(), but it allows for the creation of primary keys
#' @export
db_createTable <- function(db, tableName, dataframe = NA,
                           foreignKeys = NA,
                           addPrimary = TRUE, uniqueConstraints = NA,
                           primaryKeyName = defaultPrimaryKey(),
                           dbType = "SQLite",
                           columnDefinitions = ifelseProper(
                             identical(dataframe, NA),
                             NA,
                             db_createColumnDefinitions(dataframe,
                                                        dbType = dbType))){
  if (!identical(uniqueConstraints,NA)){
    uniqueSQL <- paste(c(", UNIQUE (",
                         paste(uniqueConstraints, collapse = ", ")
                         ,")"), collapse = "")
  } else {
    uniqueSQL <- ""
  }
  if (!identical(foreignKeys,NA)){
    foreignKeySQL <- purrr::pmap_chr(foreignKeys,
                              function(...){
                                paste(c(", FOREIGN KEY (",
                                       data.frame(...)[1,"idName"],
                                       ") REFERENCES ",
                                       data.frame(...)[1,"referenceTable"],
                                       "(",
                                       data.frame(...)[1,"referencePrimaryKey"],
                                       ")"),
                                      collapse = "")
                                }
                              )
  } else {
    foreignKeySQL <- ""
  }
  foreignKeySQL <- paste(c(uniqueSQL,foreignKeySQL), collapse ="")
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  if (addPrimary){
    if (!(primaryKeyName %in% colnames(dataframe))){
      pool::dbExecute(conn,
                paste(c("CREATE TABLE ",
                        tableName,
                        " (",
                        paste(c(paste(primaryKeyName,
                                      " INTEGER PRIMARY KEY ",
                                      ifelse(dbType == "SQLite",
                                             "AUTOINCREMENT",
                                             "AUTO_INCREMENT"),
                                      sep = ""),
                                columnDefinitions),
                              collapse = ", "),
                        foreignKeySQL,
                        ")"),
                      collapse = "")
      )
    } else {
      columnDefinitions[which(colnames(dataframe) == primaryKeyName)] <- 
        paste(columnDefinitions[which(colnames(dataframe) == primaryKeyName)], " PRIMARY KEY", sep ="")
      pool::dbExecute(conn,
                      paste(c("CREATE TABLE ",
                              tableName,
                              " (",
                              paste(columnDefinitions,
                                    collapse = ", "),
                              foreignKeySQL,
                              ")"),
                            collapse = "")
      )
    }
  } else {
    pool::dbExecute(conn,
              paste(c("CREATE TABLE ",
                      tableName,
                      " (",
                      paste(columnDefinitions, collapse = ", "),
                      foreignKeySQL,
                      ")"),
                    collapse = "")
    )
  }
  if (!identical(dataframe,NA)){
    db_writeTable(conn, tableName = tableName,
                  theTable = dataframe,
                  overwrite = FALSE, append = TRUE,
                  doNotChangePool = TRUE)
  }
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  invisible()
}

#' delete a table in a database
#'
#' @note this action cannot be undone
#'
#' @param db                database access 'handle'
#' @param tableName         name of the table
#'
#' @returns nothing
#' @export
db_deleteTable <- function(db, tableName){
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  pool::dbExecute(conn, paste(c("DROP TABLE ", tableName), collapse = ""))
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  invisible()
}

#' helper function for db_createTable(): generates a column definitions list
#' based on the column types (classes) of the data.frame
#'
#' @param dataframe data.frame which is to be written to a database using
#'                  eg db_createTable()
#' @param dbType    currently only SQLite is a valid value
#' @param dateAsInteger boolean, if FALSE the type "DATE" is used, otherwise
#'                      "INTEGER" is used. This is to circumvent data type
#'                      conventions
#' @param preferredText character string specifying which text field type is to
#'  be used. Ignored when dbType = "SQLite"
#' @param preferredBlob character string specifying which blob field type is to
#'  be used. Ignored when dbType = "SQLite"
#'
#' @return a cahracter list of column names/ column types for use as
#'         columnDefinitions in db_createTable()
#'
#' @note not all data types are in here yet, they will be added when needed
#' @note this function is experimental, may change in the future
#' @note DATE fields will come back as integer type in case of SQLite, use
#'  if as.Date(..., origin = "1970-01-01") to transform to proper dates
#' @note LOGICAL (BOOLEAN) fields will come back as integer type
#'  (0 FALSE,1 TRUE)
#' @export
db_createColumnDefinitions <- function(dataframe, dbType = "SQLite",
                                       dateAsInteger = FALSE,
                                       preferredText = "LONGTEXT",
                                       preferredBlob = "LONGBLOB"){
  if (dbType %in% c("SQLite","MySQL")){
    columns <- unlist(lapply(dataframe, function(x){class(x)[1]}))
    columnsDefinitions <- as.character()
    for (counter in 1:(length(columns))){
      columnsDefinitions[[counter]] <- paste(names(columns[counter]),
                            switch(toupper(columns[[counter]]),
                                   "NUMERIC" = "REAL",
                                   "CHARACTER" = ifelse(dbType == "SQLite",
                                                        "TEXT",
                                                        preferredText),
                                   "INTEGER" = "INTEGER",
                                   "DATE" = ifelse(dateAsInteger,
                                                   "INTEGER",
                                                   "DATE"),
                                   "LOGICAL" = "BOOLEAN",
                                   ifelse(dbType == "SQLite",  # default
                                          "BLOB",
                                          preferredBlob)
                                   ),
                            sep = " ")
    }
    return(columnsDefinitions)
  } else {
    stop("Error: database format not implemented")
  }
}

#' creates an empty data.frame base on what's in a database table
#'
#' @param db          database access 'handle'
#' @param tableName   name of the table
#' @param dbType      currently only SQLite is a valid value
#'
#' @note dates come out of database (SQLite) as TEXT field, manual conversion
#'  to POSIXCT is needed afterwards
#'
#' @returns data.frame
#' @export
db_createDataframe <- function(db, tableName, dbType = "SQLite"){
  if (dbType == "SQLite"){
    tableInfo <- db_columnInfo(db = db, tableName = tableName)
    df <- data.frame()
    for (counter in 1:(nrow(tableInfo))){
      if (tableInfo$type[counter] == "blob"){
        df <- magrittr::`%>%`(df,tibble::add_column(raw()))
      } else {
        suppressMessages(
          df <- dplyr::bind_cols(df,ifelse(tableInfo$type[counter] == "integer",
                                           as.integer(),
                                    ifelse(tableInfo$type[counter] == "numeric",
                                                  as.numeric(),
                                  ifelse(tableInfo$type[counter] == "character",
                                                         as.character(),
                                                         NULL))))
        )
      }
    }
    colnames(df) <- tableInfo$name
    return(df)
  } else {
    stop("Error: database format not implemented")
  }
}

#' gets data from a table in the database
#'
#' @param db          database access 'handle'
#' @param tableName   name of the table
#' @param SQLString   allows for more specifics actions, if tableName == NA,
#'  then SQLString is used as such, otherwise it will be used in the form of
#'  SQLString + tableName
#'
#' @returns data.frame
#' @export
db_getTable <- function(db, tableName = NA, SQLString = NA){
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  if (!identical(tableName,NA)){
    if (identical(SQLString, NA)){
      resultDF <- pool::dbGetQuery(conn = conn, paste0("SELECT * FROM ",tableName))
    } else {
      resultDF <- pool::dbGetQuery(conn = conn, paste0(SQLString, tableName))
    }
  } else {
    if (identical(SQLString, NA)){
      resultDF <- NA
    } else {
      resultDF <- pool::dbGetQuery(conn,SQLString)
    }
  }
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  return(resultDF)
}

#' gets specified columns from a table in the database, allows for where
#'  statements
#'
#' @param db          database access 'handle'
#' @param tableName   name of the table
#' @param column      if NA then all columns are retrieved, otherwise should be
#'  a character vector specifying the names of the columns to be retrieved
#' @param where       allows for conditional retrieving, should be in the form
#'  of a character vector specifying the conditions and boolean operators, eg:
#'  c("age = 45", " AND ", "gender = 'M')
#'
#' @returns data.frame
#' @export
db_getColumn <- function(db, tableName, column = NA, where = NA){
  if (identical(column,NA)){
    column = "*"
  }
  whereSQL <- ifelse(identical(where,NA),
                     "",
                     paste(c(" WHERE ",where), collapse = ""))
  return(db_getTable(db = db,
                     SQLString = paste(c("SELECT"),
                                       paste(column, collapse = ", "),
                                       "FROM",
                                       tableName,
                                       whereSQL,
                                       collapse = "")))
}

#' to perform general SQL select like statements
#'
#' @param db              database access 'handle'
#' @param SQLString       the SQL query to be performed
#' @param doNotChangePool boolean that sets whether the pool has to be locked/
#'  released inside this function. Usually this needs to be done, so default
#'  is FALSE. Set to TRUE if this locking/release is done outside of the
#'  function. Note that there is a sort of bug when working with the
#'  pool::dbWriteTable : when a sql constraint like UNIQUE is defined for the
#'  table used, this may result in a locked database upon error. It's advisable
#'  to set doNotChangePool to TRUE to prevent this from happening
#'
#' @returns result from the SQL query, usually a data.frame
#' @export
db_getSQLQuery <- function(db, SQLString, doNotChangePool = FALSE){
  if (!doNotChangePool){
    conn <- pool::poolCheckout(db)
    pool::dbBegin(conn)
  }
  result <- pool::dbGetQuery(conn, SQLString)
  if (!doNotChangePool){
    pool::dbCommit(conn = conn)
    pool::poolReturn(conn)
  }
  return(result)
}

#' to execute statements like update a row
#'
#' @param db              database access 'handle'
#' @param SQLString       the SQL query to be performed
#' @param doNotChangePool boolean that sets whether the pool has to be locked/
#'  released inside this function. Usually this needs to be done, so default
#'  is FALSE. Set to TRUE if this locking/release is done outside of the
#'  function. Note that there is a sort of bug when working with the
#'  pool::dbWriteTable : when a sql constraint like UNIQUE is defined for the
#'  table used, this may result in a locked database upon error. It's advisable
#'  to set doNotChangePool to TRUE to prevent this from happening
#' @param invisibleReturn if TRUE (default), then nothing is returned, if FALSE
#'  then the result of the SQLString will be returned (see SQL for what it can
#'  be)
#'
#' @returns result from the SQL query, usually a data.frame
#' @export
db_ExecuteSQL <- function(db, SQLString, doNotChangePool = FALSE,
                          invisibleReturn = TRUE){
  if (!doNotChangePool){
    conn <- pool::poolCheckout(db)
    pool::dbBegin(conn)
  }
  result <- pool::dbExecute(conn, SQLString)
  if (!doNotChangePool){
    pool::dbCommit(conn = conn)
    pool::poolReturn(conn)
  }
  if (!invisibleReturn){
    return(result)
  } else {
    invisible()
  }
}

#' removes rows from a table in the database specified by the 'where' argument
#'
#' @param db          database access 'handle'
#' @param tableName   name of the table
#' @param where       allows for conditional retrieving, should be in the form
#'  of a character vector specifying the conditions and boolean operators, eg:
#'  c("age = 45", " AND ", "gender = 'M'). if this parameter is NA, then nothing
#'  happens
#'
#' @returns nothing
#' @export
db_deleteRow <- function(db, tableName, where = NA){
  if (identical(where, NA)){
    invisible()
  }
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  pool::dbExecute(conn, paste(c("DELETE FROM ",
                                tableName,
                                " WHERE ",
                                paste(where, collapse ="")), collapse = ""))
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  invisible()
}

#' updates values in rows from a table in the database specified by the
#'  'where' argument
#'
#' @param db          database access 'handle'
#' @param tableName   name of the table
#' @param values      character vector specifying which columns get a new value,
#'  eg: c("age = 55", "gender = 'F'")
#' @param where       allows for conditional retrieving, should be in the form
#'  of a character vector specifying the conditions and boolean operators, eg:
#'  c("age = 45", " AND ", "gender = 'M'). if this parameter is NA, then nothing
#'  happens
#'
#' @returns nothing
#' @export
db_updateRow <- function(db, tableName, values, where = NA){
  if (identical(where, NA)){
    invisible()
  }
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  pool::dbExecute(conn, paste(c("UPDATE ",
                                tableName,
                                " SET ",
                                paste(values, collapse = ", "),
                                " WHERE ",
                                paste(where, collapse ="")), collapse = ""))
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  invisible()
}

#' creates a 'link' table for many to many relations
#'
#' @param db            database access 'handle'
#' @param tableName     name of the 'link' table to create. If NA (default),
#'  then it will create a name like this: leftTableName_rightTableName
#' @param leftTableName name of the so-called left table
#' @param leftID        name of the column in the left table to be used for the
#'  relations. This should be a unique identifier, so preferable a primary key
#' @param rightTableName name of the so-called right table
#' @param rightID        name of the column in the right table to be used for
#'  the relations. This should be a unique identifier, so preferable a primary
#'  key
#' @param addPrimary        add a autoincremented primary key (default = FALSE)
#' @param includeUniqueConstraint   if TRUE, then the two columns containing the
#'  foreign keys are set to be unique pairs. Note that this has consequences for
#'  the db_writeTable() function
#' @param primaryKeyName    name of the primary key if used (default = "id_")
#' @param dbType            default is "SQLite", only one alternative ("MySQL")
#'                          has been tested
#'
#' @note leftID is always column 1, rightID is always column 2
#'
#' @returns nothing
#' @export
db_createLinkTable <- function(db, tableName = NA,
                               leftTableName, leftID = defaultPrimaryKey(),
                               rightTableName, rightID = defaultPrimaryKey(),
                               addPrimary = FALSE,
                               includeUniqueConstraint = TRUE,
                               primaryKeyName = defaultPrimaryKey(),
                               dbType = "SQLite"){
  tableLeftID <- paste(leftTableName, leftID, sep = "_")
  tableRightID <- paste(rightTableName, rightID, sep = "_")
  if (identical(tableName, NA) | purrr::is_empty(tableName)){
    tableName <- paste(leftTableName, rightTableName, sep = "_")
  }
  columnDefinitions <- c(paste(tableLeftID, " INTEGER", sep =""),
                         paste(tableRightID, " INTEGER", sep = ""))
  foreignKeys = data.frame(
    idName = c(tableLeftID,
               tableRightID),
    referenceTable = c(leftTableName,
                       rightTableName),
    referencePrimaryKey = c(leftID,
                            rightID)
  )
  db_createTable(db = db, tableName = tableName,
                 columnDefinitions = columnDefinitions,
                 foreignKeys = foreignKeys,
                 uniqueConstraints = ifelseProper(includeUniqueConstraint,
                                            c(tableLeftID, tableRightID),
                                            NA),
                 addPrimary = FALSE, primaryKeyName = primaryKeyName,
                 dataframe = NA, dbType = dbType)
  invisible()
}

#' retrieves the IDs from the 'right' table when the IDs from the 'left' table
#'  are provided or vice versa. So gets the ID's from the other table that
#'  are 'linked'
#'
#' @param db              database access 'handle'
#' @param linkedTableName name of the 'link' table to use
#' @param hasPrimaryKey   specifies if the 'link' table has a primary key,
#'  default is FALSE
#' @param left          if TRUE then the provided ID's are from the 'left' table
#'  and the corresponding ID's from the 'right' table are retrieved. If FALSE
#'  then it's vice versa
#' @param leftID        name of the ID column in the left table to be used,
#'  if NA, the column 1 is used
#' @param rightID       name of the ID column in the right table to be used,
#'  if NA, the column 2 is used
#' @param IDs           integer vector specifying the IDs that need to be
#'  'matched'. If an ID is not present, then it will not generate anything
#'
#' @note if not specified, then leftID is always column 1, rightID is always
#'  column 2
#'
#' @returns integer vector
#' @export
db_getLinkIDs <- function(db, linkedTableName,
                          hasPrimaryKey = FALSE,
                          left = TRUE, leftID = NA, rightID = NA,
                          IDs = NA){
  fieldNames <- sum(c(identical(leftID, NA), identical(rightID, NA)))
  if (fieldNames == 1){
    stop("Error : either both column names or none should be specified")
  }
  if (fieldNames == 2){
    fieldNames <- pool::dbListFields(db, linkedTableName)
    if (hasPrimaryKey){
      fieldNames <- fieldNames[-1]
    }
  } else { # the it is 0
    fieldNames <- c(leftID, rightID)
  }
  selectSQL <- paste(c("SELECT ",
                       fieldNames[left+1],
                       " FROM ",
                       linkedTableName,
                       ifelse((identical(IDs,NA) | purrr::is_empty(IDs)),
                              "",
                              paste(c(" WHERE ",fieldNames[(!left)+1]," IN ('",
                                      paste(IDs , collapse = "','"),
                                      "')"), collapse = ""))),
                     collapse ="")
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  result <- pool::dbGetQuery(conn, selectSQL)[,1]
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  return(result)
}

#' retrieves the specified columns from the 'right' table when the IDs from
#'  the 'left' table are provided or vice versa. So gets the  columns from the
#'  other table that where the ID's are linked/matched
#'
#' @param db              database access 'handle'
#' @param linkedTableName name of the 'link' table to use
#' @param hasPrimaryKey   specifies if the 'link' table has a primary key,
#'  default is FALSE
#' @param tableName       specfies the name of the table from where to get
#'  the columns via the ID's specified by the 'link' table
#' @param columns         specifies which columns to retrieve from the table
#'  specified by tableName
#' @param primaryKey      specifies the name of the primary key of the table
#'  specified by tableName. These will be used to retrieve the right rows
#'  from the table
#' @param left            if TRUE then the provided ID's are from the 'left'
#'  table and the corresponding ID's from the 'right' table are retrieved. If
#'  FALSE then it's vice versa
#' @param leftID          name of the ID column in the left table to be used,
#'  if NA, the column 1 is used
#' @param rightID         name of the ID column in the right table to be used,
#'  if NA, the column 2 is used
#' @param IDs             integer vector specifying the IDs that need to be
#'  'matched'. If an ID is not present, then it will not generate anything
#'
#' @note if not specified, then leftID is always column 1, rightID is always
#'  column 2
#'
#' @returns integer vector
#' @export
db_getLinkColumns <- function(db, linkedTableName, hasPrimaryKey = FALSE,
                              tableName, columns = NA,
                              primaryKey = defaultPrimaryKey(),
                              left = TRUE, leftID = NA, rightID = NA,
                              IDs = NA){
  fieldNames <- sum(c(identical(leftID,NA), identical(rightID,NA)))
  if (fieldNames == 1){
    stop("Error : either both column names or none should be specified")
  }
  if (fieldNames == 2){
    fieldNames <- pool::dbListFields(db, linkedTableName)
    if (hasPrimaryKey){
      fieldNames <- fieldNames[-1]
    }
  } else { # the it is 0
    fieldNames <- c(leftID, rightID)
  }
  selectSQL <- paste(c("SELECT ",
                       ifelse(identical(columns, NA),
                              "*",
                              paste(columns,collapse = ", ")),
                       " FROM ",
                       tableName,
                       " WHERE ",
                       primaryKey,
                       " IN ",
                       paste(c("(SELECT ",
                               fieldNames[left+1],
                               " FROM ",
                               linkedTableName,
                               ifelse((identical(IDs,NA) | purrr::is_empty(IDs)),
                                      "",
                                      paste(c(" WHERE ",fieldNames[(!left)+1]," IN ('",
                                              paste(IDs , collapse = "','"),
                                              "')"), collapse = "")),
                               ")"),
                             collapse ="")
  ), collapse ="")
  conn <- pool::poolCheckout(db)
  pool::dbBegin(conn)
  result <- pool::dbGetQuery(conn, selectSQL)
  pool::dbCommit(conn = conn)
  pool::poolReturn(conn)
  return(result)
}

#' Copies a single table from one database to another (only sqlite type
#'  databases have been tested). Internal function
#'
#' @param dbFrom database from which tables are to be copied, must be open when
#'  calling this function
#' @param dbTo database to which tables are to be copied, must be open when
#'  calling this function
#' @param noWarnings suppress warnings while copying data, default = TRUE
#' @param tableName character vector specifying which table to copy
#' @return nothing
#' @note internal function
copyTable <- function(dbFrom, dbTo, tableName, noWarnings = TRUE){
  if (noWarnings){
    suppressWarnings({
      tempTibble <- dplyr::tbl(dbFrom, tableName) %>% dplyr::collect()
      pool::dbWriteTable(dbTo, tableName, tempTibble)
    })
  } else {
    tempTibble <- dplyr::tbl(dbFrom, tableName) %>% dplyr::collect()
    pool::dbWriteTable(dbTo, tableName, tempTibble)
  }
}

#' copies contents of a sqlite database to another one (default is into
#'  :memory:)
#' @param dbFrom database from which tables are to be copied, must be open when
#'  calling this function
#' @param dbTo database to which tables are to be copied
#' @param noWarnings suppress warnings while copying data, default = TRUE
#' @param copyTables character vector specifying which tables to copy (default
#'  NA, copies all tables)
#' @param doNotCopyTables character vector that specifies which tables are not
#'  to be copied (default = 'sqlite_sequence'). Overrules the copyTables
#'  argument. If NA then this parameter is ignored
#' @return an in memory database database, note that this database is returned
#'  in an 'open state'
#' @export
copyDBtoMemory <- function(dbFrom, dbTo = ":memory:", noWarnings = TRUE, copyTables = NA, doNotCopyTables = "sqlite_sequence"){
  if (identical(copyTables, NA)){
    copyTables <- pool::dbListTables(dbFrom)
  }
  if (!identical(doNotCopyTables, NA)){
    toRemove <- which(copyTables %in% doNotCopyTables)
    if (length(toRemove) > 0){
      copyTables <- copyTables[-toRemove]
    }
  }
  dbTo <- db_open(dbTo)
  if (length(copyTables) > 0){
    purrr::walk(copyTables, ~copyTable(dbFrom = dbFrom,
                                       dbTo = dbTo,
                                       tableName = .x,
                                       noWarnings = noWarnings))
  }
  return(dbTo)
}