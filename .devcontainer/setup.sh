#!/bin/bash
set -e

git config --global --unset-all core.sshCommand || true

Rscript -e '
  pkgs <- c("languageserver", "httpgd", "jsonlite", "lintr", "diffviewer", "vdiffr")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) install.packages(missing)
  BiocManager::install(
    c(
      "ggfortify",
      "SummarizedExperiment",
      "assertthat",
      "Biobase",
      "BiocGenerics",
      "biomaRt",
      "checkmate",
      "circlize",
      "ComplexHeatmap",
      "DESeq2",
      "DT",
      "dplyr",
      "edgeR",
      "EnvStats",
      "forcats",
      "GenomicRanges",
      "ggplot2",
      "ggrepel",
      "IRanges",
      "limma",
      "matrixStats",
      "MultiAssayExperiment",
      "Rdpack",
      "S4Vectors",
      "tidyr",
      "BiocStyle"
    ),
    ask = FALSE,
    update = TRUE
  )
'
