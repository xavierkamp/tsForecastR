% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arg_checks.R
\name{check_time_id}
\alias{check_time_id}
\title{Check the time identifier}
\usage{
check_time_id(time_id)
}
\arguments{
\item{time_id}{A POSIXct, timestamp created with \code{\link[base]{Sys.time}} which is then appended to the results}
}
\value{
A POSIXct, timestamp
}
\description{
This function ensures that the user specifies a valid time identifier.
}
