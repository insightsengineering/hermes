# hermes 1.0.1.9012

### Enhancements
* New plotting function `draw_heatmap` to produce heatmaps of (normalized) counts.

### Miscellaneous
* The utility function `df_cols_to_factor` now also converts existing factors to having explicit missing levels.
* Version bump on `forcats` dependency.

# hermes 1.0.1

### Enhancements
* Additional `version` argument for `connect_biomart` to specify an Ensembl version.

# hermes 1.0.0

### Meta
* First public release of the `hermes` package.
* Submission to BioConductor.

### Enhancements
* Better legends on the genes barplot and the correlation heatmap.
* Improved vignette layout using the BioConductor style.

### Miscellaneous
* New utility function `cut_quantile` for cutting a numeric vector into quantiles.
* New utility function `cat_with_newline` for concatenating and printing with newline.
* New check function `check_proportion` which checks for a single proportion.

# hermes 0.1.1

### New Features
* New function `draw_scatterplot` to produce scatterplots of two genes or gene signatures.
* New function `draw_boxplot` for boxplots of gene expression values.
* New function `draw_barplot` for barplots of dichotomized gene expression counts into two or three percentile categories.
* New helper function `wrap_in_mae` that wraps a single `SummarizedExperiment` object into an MAE object.
* New method `rename` that makes renaming columns of `rowData` and `colData` as well as assay names in existing `SummarizedExperiment` objects much easier, as a step before converting to `HermesData`.
* New method `lapply` that allows user to apply a function on all experiments in a `MultiAssayExperiment`.
* New method `isEmpty` that checks whether a `SummarizedExperiment` object is empty.
* New gene filtering option `n_top` in the `calc_pca` function, which allows filtering genes with greatest variability across samples.
* New class `GeneSpec` for specification of genes or gene signatures, see `?gene_spec` for simple construction. Inclusion of gene signature functions `colPrinComp1` and `colMeanZscores` to supplement standard column statistics functions.
* New helper function `col_data_with_genes` which extracts the sample variables saved in `colData` together with selected gene information as a combined data set.
* New helper function `inner_join_cdisc` which joins genetic with CDISC data sets.

### Bug Fixes
* `normalize()` now also works when the `hermes` package is not loaded, i.e. you can use it with `hermes::normalize()`.
* `correlate()` now also works when there are factor variables in the sample variables of the `HermesData` object.
* `add_quality_flags()` does no longer return `NA` as the technical failure flags for the samples if there is only a single gene contained in the input, but instead a vector of `FALSE` to ensure correct downstream functionality.

### Miscellaneous
* Updated `LICENCE` and `README` with new package references.
* The `multi_assay_experiment` now contains `HermesData` experiments, different patient IDs, one experiment with normalized assays, and multiple samples per patient in one experiment.
* The main `HermesData` example is now saved in the package as `hermes_data`, and the previous `summarized_experiment` is still available. Note that patient IDs have been changed in the new version to align with the `multi_assay_experiment`.
* Renaming of required `rowData` and `colData` columns to be more consistent with standards and use lowercase snake-case names.
* Annotation querying and setting is now more flexible in that it also allows to query more annotations than the required ones.
* Instead of gene starts and ends, the total length of gene exons is now used as the annotation column `size`. Corresponding queries from BioMart are used to return this gene size.
* `df_char_to_factor` has been deprecated (and can still be used with a warning) and replaced with `df_cols_to_factor`, which also converts logical variables to factor variables.
* When providing `SummarizedExperiment` objects containing `DelayedMatrix` assays to the `HermesData()` constructor, these are silently converted to `matrix` assays to ensure downstream functionality.

# hermes 0.1.0
* First internal release of the `hermes` package, which contains classes, methods and functions to import, quality-check, filter, normalize, and analyze RNAseq counts data for differential expression.
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
