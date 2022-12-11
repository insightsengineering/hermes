<!-- markdownlint-disable-file -->

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering//actions/workflows/check.yaml/badge.svg)](https://github.com/insightsengineering//actions/workflows/check.yaml)
[![Docs 📚](https://github.com/insightsengineering//actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io//)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering//_xml_coverage_reports/data/main/coverage.xml)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering//main?color=purple\&label=package%20version)](https://github.com/insightsengineering//tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/?color=red\&label=open%20issues)](https://github.com/insightsengineering//issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

<!-- markdownlint-disable-file -->
<!-- README.md is generated from README.Rmd. Please edit that file -->
# hermes
<p align="center">
<img src='man/figures/logo.png' align="right" height="131.5" alt="hermes-logo"/>
</p>

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Code Coverage](https://raw.githubusercontent.com/insightsengineering/hermes/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/hermes/_xml_coverage_reports/data/main/coverage.xml)

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

### GitHub

You can install the latest stable release from GitHub with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("insightsengineering/hermes", ref = "v0.1.1")
```

### BioConductor

Currently `hermes` is on development version 3.15 of BioConductor, which
requires development R 4.2. Please be aware of this. The actual release will
happen when 3.15 is released, expected in Q2 2022.

If you like you can install the current development version from BioConductor with:

``` r
if (!require("BiocManager")) {
  install.packages("BiocManager")
}
BiocManager::install("hermes")
```

## Getting Started

You can get started by reading the introduction vignette:

``` r
library(hermes)
vignette("introduction", package = "hermes")
```

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/hermes.svg)](https://starchart.cc/insightsengineering/hermes)

### Stargazers

[![Stargazers repo roster for @insightsengineering/hermes](https://reporoster.com/stars/insightsengineering/hermes)](https://github.com/insightsengineering/hermes/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/hermes](https://reporoster.com/forks/insightsengineering/hermes)](https://github.com/insightsengineering/hermes/network/members)
