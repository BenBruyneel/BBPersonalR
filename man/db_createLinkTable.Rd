% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbAccess.R
\name{db_createLinkTable}
\alias{db_createLinkTable}
\title{creates a 'link' table for many to many relations}
\usage{
db_createLinkTable(
  db,
  tableName = NA,
  leftTableName,
  leftID = defaultPrimaryKey(),
  rightTableName,
  rightID = defaultPrimaryKey(),
  addPrimary = FALSE,
  includeUniqueConstraint = TRUE,
  primaryKeyName = defaultPrimaryKey(),
  dbType = "SQLite"
)
}
\arguments{
\item{db}{database access 'handle'}

\item{tableName}{name of the 'link' table to create. If NA (default),
then it will create a name like this: leftTableName_rightTableName}

\item{leftTableName}{name of the so-called left table}

\item{leftID}{name of the column in the left table to be used for the
relations. This should be a unique identifier, so preferable a primary key}

\item{rightTableName}{name of the so-called right table}

\item{rightID}{name of the column in the right table to be used for
the relations. This should be a unique identifier, so preferable a primary
key}

\item{addPrimary}{add a autoincremented primary key (default = FALSE)}

\item{includeUniqueConstraint}{if TRUE, then the two columns containing the
foreign keys are set to be unique pairs. Note that this has consequences for
the db_writeTable() function}

\item{primaryKeyName}{name of the primary key if used (default = "id_")}

\item{dbType}{default is "SQLite", only one alternative ("MySQL")
has been tested}
}
\value{
nothing
}
\description{
creates a 'link' table for many to many relations
}
\note{
leftID is always column 1, rightID is always column 2
}
