# hermes 0.1.0.9000

### New Features
* Scatterplot of the two genes or gene signatures via `draw_scatterplot`.
* Boxplot of the gene expression values via `draw_boxplot`. Faceting, stratifying, and coloring by sample variables, as well as comparison of multiple genes is supported.
* The `multi_assay_experiment` now contains `HermesData` experiments, different patient IDs, one experiment with normalized assays, and multiple samples per patient in one experiment.
* `calc_pca` now includes a top number of gene filtering option `n_top`, which allows filtering genes with greatest variability across samples. 
* Generation of gene signatures with the functions `colPrinComp1` and `colMeanZscores` using the PC1 and the Z-score respectively.
* Renaming of required `rowData` and `colData` columns to be more consistent with standards and use lowercase snake-case names.
* Annotation querying and setting is now more flexible in that it also allows to query more annotations than the required ones.
* `df_chars_to_factor` has been deprecated (and can still be used with a warning) and replaced with `df_cols_to_factor`, which also converts logical variables to factor variables.

### Bug Fixes
* `normalize()` now also works when the `hermes` package is not loaded, i.e. you can use it with `hermes::normalize()`.

### Miscellaneous
* Added `error_on_lint: TRUE` to `.lintr`.

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
