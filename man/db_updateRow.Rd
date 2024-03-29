% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbAccess.R
\name{db_updateRow}
\alias{db_updateRow}
\title{updates values in rows from a table in the database specified by the
 'where' argument}
\usage{
db_updateRow(db, tableName, values, where = NA)
}
\arguments{
\item{db}{database access 'handle'}

\item{tableName}{name of the table}

\item{values}{character vector specifying which columns get a new value,
eg: c("age = 55", "gender = 'F'")}

\item{where}{allows for conditional retrieving, should be in the form
of a character vector specifying the conditions and boolean operators, eg:
c("age = 45", " AND ", "gender = 'M'). if this parameter is NA, then nothing
happens}
}
\value{
nothing
}
\description{
updates values in rows from a table in the database specified by the
 'where' argument
}
