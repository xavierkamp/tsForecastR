% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_management.R
\name{get_split_keys}
\alias{get_split_keys}
\title{Get split keys}
\usage{
get_split_keys(sample_split)
}
\arguments{
\item{sample_split}{A list, the sample split (e.g. training, validation and test sets)}
}
\value{
A data.frame with split keys
}
\description{
This function identifies which observations belong to which set (e.g. training, validation, test sets) by creating
a split identifier.
}
