% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connections.R
\name{h_strip_prefix}
\alias{h_strip_prefix}
\title{Stripping Prefix from Gene IDs}
\usage{
h_strip_prefix(gene_ids, prefix)
}
\arguments{
\item{gene_ids}{(\code{character})\cr original gene IDs including prefix and optional
delimiter before the digits.}

\item{prefix}{(\code{string})\cr common prefix to be stripped away from \code{gene_ids}.}
}
\value{
Character vector that contains only the digits for each gene ID.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This helper function removes the prefix and possible delimiter from
a vector of gene IDs, such that only the digits are returned.
}
\note{
This is currently used to strip away the \code{GeneID} prefix from \code{Entrez} gene IDs
so that they can be queried from \code{BioMart}
}
\examples{
h_strip_prefix(c("GeneID:11185", "GeneID:10677"), prefix = "GeneID")
}
