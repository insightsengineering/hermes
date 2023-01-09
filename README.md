
<!-- markdownlint-disable-file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
# hermes
<p align="center">
<img src='man/figures/logo.png' align="right" height="131.5" alt="hermes-logo"/>
</p>
<!-- start badges -->

[![Check
🛠](https://github.com/insightsengineering/hermes/actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering/hermes/actions/workflows/check.yaml)
[![Docs
📚](https://github.com/insightsengineering/hermes/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/hermes/)
[![Code Coverage
📔](https://raw.githubusercontent.com/insightsengineering/hermes/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/hermes/_xml_coverage_reports/data/main/coverage.xml)

![GitHub
forks](https://img.shields.io/github/forks/insightsengineering/hermes?style=social)
![GitHub Repo
stars](https://img.shields.io/github/stars/insightsengineering/hermes?style=social)

![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/insightsengineering/hermes)
![GitHub
contributors](https://img.shields.io/github/contributors/insightsengineering/hermes)
![GitHub last
commit](https://img.shields.io/github/last-commit/insightsengineering/hermes)
![GitHub pull
requests](https://img.shields.io/github/issues-pr/insightsengineering/hermes)
![GitHub repo
size](https://img.shields.io/github/repo-size/insightsengineering/hermes)
![GitHub language
count](https://img.shields.io/github/languages/count/insightsengineering/hermes)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current
Version](https://img.shields.io/github/r-package/v/insightsengineering/hermes/main?color=purple&label=package%20version)](https://github.com/insightsengineering/hermes/tree/main)
[![Open
Issues](https://img.shields.io/github/issues-raw/insightsengineering/hermes?color=red&label=open%20issues)](https://github.com/insightsengineering/hermes/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->  

> `hermes` facilitates preprocessing, analyzing, and reporting of
> RNA-seq data.

- Imports RNAseq count data into the `hermes` ready format.
- Annotates gene information automatically from a central database
  (e.g. BioMart).
- Adds quality control (QC) flags to genes and samples.
- Filters the data set.
- Normalizes the counts.
- Quickly produces descriptive plots.
- Performs principal components analysis.
- Produces a templated QC report.
- Performs differential expression analysis.

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
