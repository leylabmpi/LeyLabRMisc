% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivar.R
\name{qsave_obj}
\alias{qsave_obj}
\title{Simple function for serializing a distance matrix or list of distance matrices}
\usage{
qsave_obj(x, file, msg = "Writing file to: ", threads = 1)
}
\arguments{
\item{x}{a distance matrix or list of distance matrices}

\item{file}{file name to save to}

\item{threads}{number of threads used for serializing}
}
\value{
the input distance matrix or list of distance matrices
}
\description{
Serializing done with the "qs" R package.
}
