% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updating_functions.R
\name{update_session_dates}
\alias{update_session_dates}
\title{Function to update \code{`sessions`}'s \code{data_start} and \code{date_end} 
variables in a vehicle emissions database.}
\usage{
update_session_dates(con)
}
\arguments{
\item{con}{Database connection to a vehicle emissions database.}
}
\value{
Invisible, a database insert.
}
\description{
Function to update \code{`sessions`}'s \code{data_start} and \code{date_end} 
variables in a vehicle emissions database.
}
\author{
Stuart K. Grange
}
