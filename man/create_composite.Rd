% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_composite.R
\name{create_composite}
\alias{create_composite}
\title{Create Composites}
\usage{
create_composite(data, selection, name, na.rm = T)
}
\arguments{
\item{data}{The data to add a composite column to}

\item{selection}{A tidy selection of the variables that are to be averaged together}

\item{name}{The name of the variable to be created}

\item{na.rm}{Set to TRUE (default), will adjust the denominator in the case of missing data. If set to false, will return NA if the row has any missing data}
}
\value{
a tibble object
}
\description{
This function will take as input a dataset, a selection of columns, and a name for the column to be created
}
\examples{
mtcars |> create_composite(c(drat, gear), drat_gear_average)
}
