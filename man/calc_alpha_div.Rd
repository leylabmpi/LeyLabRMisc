% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multivar.R
\name{calc_alpha_div}
\alias{calc_alpha_div}
\title{Calculate common alpha-diversity metrics
You need the "vegan" package installed to your R project and loaded for this code to run}
\usage{
calc_alpha_div(df, tree = NULL, index = c("nobs", "shannon", "PD"))
}
\arguments{
\item{df}{sample x taxon abundance table (usual format for vegan)}

\item{tree}{tree with tips matching taxa in the abundance table (only needed for PD)}

\item{index}{which of the indices to calculate? (nobs = no. of observations, shannon = Shannon Index, PD = Faith's PD)}
}
\value{
a data.frame of alpha diversity values (and sample names)
}
\description{
Faith's Phylogenetic Diversity ("PD") can be calculated only
if a tree is provided. The tree can have extra tips, but there
must be tip labels for all taxa in the provided table.
}
