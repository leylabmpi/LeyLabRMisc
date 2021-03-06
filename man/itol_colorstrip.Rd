% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itol.R
\name{itol_colorstrip}
\alias{itol_colorstrip}
\title{create itol colorstrip file}
\usage{
itol_colorstrip(df, dataset_label, out_file, out_dir = NULL, legend = NULL)
}
\arguments{
\item{df}{Dataframe, in which the rownames should correspond with the tree labels; the plotting parameter should be column 1}

\item{dataset_label}{What to label the itol dataset}

\item{out_file}{Name of the output file}

\item{out_dir}{Where to write the output}

\item{legend}{Custom legend (see the function description)}
}
\description{
https://itol.embl.de/help.cgi#strip
}
\details{
Custom Legend: requires a data.frame with the number of rows equaling the number of unique
values in the legend.
\itemize{
  \item "shapes" => numeric (see \href{https://itol.embl.de/help.cgi#dsLeg}{the itol docs})
  \item "colors" => hexidecimal (see \href{https://www.color-hex.com/}{this website for examples})
  \item "labels" => legend labels
}
}
\examples{
# creating a custom legend
legend = data.frame(unique(iris$Species),
colors = c('#00FF00', '#FFCC33', '#FF0000'),
shapes = rep(1, length(unique(iris$Species))))
legend
}
