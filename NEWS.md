# hermes 0.1.0.9000

### New Features
* Scatterplot of the gene expression values of two genes via `draw_scatterplot`.
* Boxplot of the gene expression values of a gene for a sample variable via `draw_boxplot`.

# hermes 0.1.0
* First release of the `hermes` package, which contains classes, methods and functions to import, quality-check, filter, normalize, and analyze RNAseq counts data for differential expression. 
* `hermes` is a successor of the `rnaseqTools` R package. The core functionality is built on the BioConductor ecosystem, especially the `SummarizedExperiment` class. New users should first begin by reading the "Introduction to `hermes`" vignette to become familiar with the `hermes` concepts.

### New Features
* Import RNAseq count data into the `hermes` ready format.
* Annotate gene information from the Ensembl database via `biomaRt`.
* Add quality control (QC) flags to genes and samples.
* Filter and subset the data set.
* Normalize the counts.
* Produce descriptive plots.
* Perform principal components analysis.
* Produce a templated QC Rmd report.
* Perform differential expression analysis.
