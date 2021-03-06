% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/large_files.R
\name{large_summary}
\alias{large_summary}
\title{Generate and Print summary statistics for a dataset that can't be held in memory}
\usage{
large_summary(path, display = T, verbose = F)
}
\arguments{
\item{path}{path to a csv file representing a dataframe}

\item{display}{boolean, should the summary be printed to the console or returned as a list?}
}
\description{
Generate and Print summary statistics for a dataset that can't be held in memory
}
\details{
If display = T (the default), nothing will be returned but the output will be formatted and printed.
If display = F, then the summary statistics will be returned as a list, where the name of each element is a variable
and the contents are a list of the summary statistics calculated for each data type.
}
