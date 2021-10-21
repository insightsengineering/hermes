# hermes 0.1.0.9000

### New Features
* Scatterplot of the two genes or gene signatures via `draw_scatterplot`.
* Boxplot of the gene expression values via `draw_boxplot`. Faceting, stratifying, and coloring by sample variables, as well as comparison of multiple genes is supported.
* The `multi_assay_experiment` now contains `HermesData` experiments, different patient IDs, one experiment with normalized assays, and multiple samples per patient in one experiment.
* `calc_pca` now includes a top number of gene filtering option `n_top`, which allows
filtering genes with greatest variability across samples. 

### Bug Fixes
* `normalize()` now also works when the `hermes` package is not loaded, i.e. you can use it with `hermes::normalize()`.

### Miscellaneous
* Added `error_on_lint: TRUE` to `.lintr`.
* New utility function `cat_with_newline` for concatenating and printing with newline.

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
