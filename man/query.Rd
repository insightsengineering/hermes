% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connections.R
\name{query}
\alias{query}
\alias{query,character,ConnectionBiomart-method}
\title{Query Gene Annotations from a Connection}
\usage{
query(genes, connection)

\S4method{query}{character,ConnectionBiomart}(genes, connection)
}
\arguments{
\item{genes}{(\code{character})\cr gene IDs.}

\item{connection}{(connection class)\cr data base connection object.}
}
\value{
A \code{\link[S4Vectors:DataFrame-class]{S4Vectors::DataFrame}} with the gene annotations. It is required that:
\itemize{
\item The \code{rownames} are identical to the input \code{genes}.
\item The \code{colnames} are equal to the annotation columns \code{\link{.row_data_annotation_cols}}.
\item Therefore, missing information needs to be properly included in the \code{DataFrame}
with \code{NA} entries.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

The generic function \code{query()} is the interface for querying gene annotations from
a data base connection.
}
\details{
\itemize{
\item A method is provided for the \code{\link{ConnectionBiomart}} class. However, the framework
is extensible: It is simple to add new connections and corresponding query methods
for other data bases, e.g. company internal data bases. Please make sure to
follow the required format of the returned value.
\item The \code{BioMart} queries might not return information for all the genes. This can be
due to different versions being used in the gene IDs and the queried \code{Ensembl} data base.
}
}
\examples{
if (interactive()) {
  object <- hermes_data
  connection <- connect_biomart(prefix(object))
  result <- query(genes(object), connection)
  head(result)
  head(annotation(object))
}
}
