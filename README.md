<!-- markdownlint-disable-file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
# hermes
<p align="center">
<img src='man/figures/logo.png' align="right" height="131.5" alt="hermes-logo"/>
</p>

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)  

> `hermes` facilitates preprocessing, analyzing, and reporting of
> RNA-seq data.

-   Imports RNAseq count data into the `hermes` ready format.
-   Annotates gene information automatically from a central database
    (e.g. BioMart).
-   Adds quality control (QC) flags to genes and samples.
-   Filters the data set.
-   Normalizes the counts.
-   Quickly produces descriptive plots.
-   Performs principal components analysis.
-   Produces a templated QC report.
-   Performs differential expression analysis.

## Installation

### BioConductor

You can install the current release from BioConductor with:

``` r
if (!require("BiocManager")) {
  install.packages("BiocManager")
}
BiocManager::install("hermes")
```

### GitHub

You can install the development version from GitHub with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("insightsengineering/hermes")
```

## Getting Started

You can get started by reading the introduction vignette:

``` r
library(hermes)
vignette("introduction", package = "hermes")
```
