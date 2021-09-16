% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{df_cols_to_factor}
\alias{df_cols_to_factor}
\title{Conversion of Character to Factor Variables in a \code{DataFrame}}
\usage{
df_cols_to_factor(data, omit_columns = NULL, na_level = "<Missing>")
}
\arguments{
\item{data}{(\code{DataFrame})\cr input \code{\link[S4Vectors:DataFrame-class]{S4Vectors::DataFrame}}.}

\item{omit_columns}{(\code{character} or \code{NULL})\cr which columns should be omitted from
the conversion.}

\item{na_level}{(\code{string})\cr missing level to be used.}
}
\value{
The modified data.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This utility function converts all character variables in a \code{\link[S4Vectors:DataFrame-class]{S4Vectors::DataFrame}}
to factor variables with explicit missing level.
}
\details{
This is using \code{\link[tern:df_explicit_na]{tern::df_explicit_na()}} which only works for classic \code{\link{data.frame}}
objects. We avoid a conversion of the whole \code{data} to \code{\link{data.frame}} since that could be
problematic when not supported classes are used in other non-character columns.
}
\examples{
dat <- colData(summarized_experiment)
any(sapply(dat, is.character))
any(sapply(dat, is.logical))
dat_converted <- df_cols_to_factor(dat)
any(sapply(dat_converted, function(x) is.character(x) || is.logical(x)))
}