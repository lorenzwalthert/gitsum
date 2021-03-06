% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testing.R
\name{parse_test_log}
\alias{parse_test_log}
\title{Parse a test log}
\usage{
parse_test_log(path_to_raw_log, parser = parse_log_detailed_full_run,
  ...)
}
\arguments{
\item{path_to_raw_log}{The path to the raw log relative to tests/testthat/.}

\item{parser}{The parser to use, i.e. either \code{\link[=parse_log_detailed_full_run]{parse_log_detailed_full_run()}}
or \code{\link[=parse_log_simple]{parse_log_simple()}}.}

\item{...}{Params passed to \code{\link[=parse_log_detailed_full_run]{parse_log_detailed_full_run()}}.}
}
\description{
Function to facilitate the reading and parsing of a raw log for unit test
purposes.
}
\keyword{internal}
