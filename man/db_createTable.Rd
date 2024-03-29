% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbAccess.R
\name{db_createTable}
\alias{db_createTable}
\title{create a table in a database and (optionally) put data in it}
\usage{
db_createTable(
  db,
  tableName,
  dataframe = NA,
  foreignKeys = NA,
  addPrimary = TRUE,
  uniqueConstraints = NA,
  primaryKeyName = defaultPrimaryKey(),
  dbType = "SQLite",
  dateAsInteger = FALSE,
  preferredText = "LONGTEXT",
  preferredBlob = "LONGBLOB",
  columnDefinitions = ifelseProper(identical(dataframe, NA), NA,
    db_createColumnDefinitions(dataframe, dbType = dbType, dateAsInteger = dateAsInteger,
    preferredText = preferredText, preferredBlob = preferredBlob))
)
}
\arguments{
\item{db}{database access 'handle'}

\item{tableName}{name of the table}

\item{dataframe}{data.frame containing data to be written to the
newly created database}

\item{foreignKeys}{data.frame with columns idName, referenceTable,
referencePrimaryKey. This will give per row this
SQL synthax FOREIGN KEY (idName) REFERENCES
referenceTable(referencePrimaryKey). If set to
NA (default) then no foreign keys are added}

\item{addPrimary}{add a autoincremented primary key (default = TRUE)}

\item{uniqueConstraints}{allows the setting of (SQL) UNIQUE constraints of
the form UNIQUE(..., ...). If NA, no constraints are
set, otherwise it should be a character vector
specifying the column names to be set as unique}

\item{primaryKeyName}{name of the primary key if used (default = "id_")
note: if this argument is the name of an existing column, the primary key
will not be added, but the column will be set to 'primary key'. The
primary key will not be set to autoincrement in this case!}

\item{dbType}{default is "SQLite", only one alternative ("MySQL")
has been tested}

\item{dateAsInteger}{boolean, if FALSE the type "DATE" is used, otherwise
"INTEGER" is used. This is to circumvent data type
conventions}

\item{preferredText}{character string specifying which text field type is to
be used. Ignored when dbType = "SQLite"}

\item{preferredBlob}{character string specifying which blob field type is to
be used. Ignored when dbType = "SQLite"}

\item{columnDefinitions}{vector of character strings with each string of the
format "columnName columnType", eg
c("filename TEXT","fileversion TEXT")}
}
\value{
nothing
}
\description{
create a table in a database and (optionally) put data in it
}
\note{
this may seem somewhat cumbersome way to create a table, when compared
to dplyr::copy_to(), but it allows for the creation of primary keys
}
