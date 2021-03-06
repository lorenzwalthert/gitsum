% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{check_overwriting_clearance}
\alias{check_overwriting_clearance}
\title{Check whether a file / path exists and return an error when it cannot be
overwritten.}
\usage{
check_overwriting_clearance(path, over_write, fun = file.exists)
}
\arguments{
\item{path}{The path to the root directory of the gitsum repository.}

\item{over_write}{Whether or not existing files / directories should be
overwritten.}

\item{fun}{The function to apply to the path, either file.exists, or
dir.exists.}
}
\description{
Check whether a file / path exists and return an error when it cannot be
overwritten.
}
\keyword{internal}
