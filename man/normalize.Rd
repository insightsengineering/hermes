% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization.R
\name{normalize,AnyHermesData-method}
\alias{normalize,AnyHermesData-method}
\alias{normalize}
\alias{h_cpm}
\alias{h_rpkm}
\alias{h_tpm}
\alias{h_voom}
\alias{h_vst}
\alias{h_rlog}
\title{Normalization of \code{AnyHermesData} Objects}
\usage{
\S4method{normalize}{AnyHermesData}(
  object,
  methods = c("cpm", "rpkm", "tpm", "voom", "vst"),
  control = control_normalize(),
  ...
)

h_cpm(object, control = control_normalize())

h_rpkm(object, control = control_normalize())

h_tpm(object, control = control_normalize())

h_voom(object, control = control_normalize())

h_vst(object, control = control_normalize())

h_rlog(object, control = control_normalize())
}
\arguments{
\item{object}{(\code{AnyHermesData})\cr object to normalize.}

\item{methods}{(\code{character})\cr which normalization methods to use, see details.}

\item{control}{(named \code{list})\cr settings produced by \code{\link[=control_normalize]{control_normalize()}}.}

\item{...}{not used.}
}
\value{
The \code{\link{AnyHermesData}} object with additional assays containing the normalized counts.
The \code{control} is saved in the \code{metadata} of the object for future reference.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

The \code{normalize()} method is normalizing the input \code{\link{AnyHermesData}} according to one or more
specified normalization methods. The results are saved as additional assays
in the object.

Possible normalization methods (which are implemented with separate helper functions):
\itemize{
\item cpm: Counts per Million (\code{CPM}). Separately by sample, the original counts of the genes
are divided by the library size of this sample, and multiplied by one million. This is the
appropriate normalization for between-sample comparisons.
\item rpkm: Reads per Kilobase of transcript per Million reads mapped (RPKM). Each gene count is
divided by the gene size (in kilobases) and then again divided by the library sizes of each
sample (in millions). This allows for within-sample comparisons, as it takes
into account the gene sizes - longer genes will always have more counts than shorter genes.
\item tpm: Transcripts per Million (TPM). This addresses the problem of RPKM being inconsistent
across samples (which can be seen that the sum of all RPKM values will vary from sample to
sample). Therefore here we divide the RPKM by the sum of all RPKM values for each sample,
and multiply by one million.
\item voom: VOOM normalization. This is essentially just a slight variation of \code{CPM} where
a \code{prior_count} of 0.5 is combined with \code{lib_sizes} increased by 1 for each sample. Note that
this is not required for the corresponding differential expression analysis, but just provided
as a complementary experimental normalization approach here.
\item \code{vst}: Variance stabilizing transformation. This is to transform the normalized
count data for all genes into approximately homoskedastic values (having constant variance).
\item \code{rlog}: The transformation to the log2 scale values with approximately homoskedastic values.
}
}
\section{Functions}{
\itemize{
\item \code{h_cpm()}: calculates the Counts per Million (\code{CPM}) normalized counts.

\item \code{h_rpkm()}: calculates the Reads per Kilobase per Million (RPKM) normalized counts.

\item \code{h_tpm()}: calculates the Transcripts per Million (TPM) normalized counts.

\item \code{h_voom()}: calculates the VOOM normalized counts. \ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\item \code{h_vst()}: variance stabilizing transformation (\code{vst}) from \code{DESeq2} package.

\item \code{h_rlog()}: regularized log transformation (\code{rlog}) from \code{DESeq2} package.

}}
\examples{
a <- hermes_data

# By default, log values are used with a prior count of 1 added to original counts.
result <- normalize(a)
assayNames(result)
tpm <- assay(result, "tpm")
tpm[1:3, 1:3]

# We can also work on original scale.
result_orig <- normalize(a, control = control_normalize(log = FALSE))
tpm_orig <- assay(result_orig, "tpm")
tpm_orig[1:3, 1:3]

# Separate calculation of the CPM normalized counts.
counts_cpm <- h_cpm(a)
str(counts_cpm)

# Separate calculation of the RPKM normalized counts.
counts_rpkm <- h_rpkm(a)
str(counts_rpkm)

# Separate calculation of the TPM normalized counts.
counts_tpm <- h_tpm(a)
str(counts_tpm)

# Separate calculation of the VOOM normalized counts.
counts_voom <- h_voom(a)
str(counts_voom)

# Separate calculation of the vst transformation.
counts_vst <- h_vst(a)
str(counts_vst)

# Separate calculation of the rlog transformation.
counts_rlog <- h_rlog(a)
str(counts_rlog)
}
\seealso{
\code{\link[=control_normalize]{control_normalize()}} to define the normalization method settings.
}
