setMethod(f = "rbind", 
          signature = c("SummarizedExperiment"),
          function(..., deparse.level = 1)
          {
           args <- unname(list(...)) #removes names and dimnames of the object
           .rbind.SummarizedExperiment(args)
          })

# rbind function ----
.rbind.SummarizedExperiment <- function(args) 
  {
  SE_check <- unlist(lapply(args, function(x) "SummarizedExperiment" %in% class(x)))
  if (!all(SE_check)) stop("Not all input objects are SummarizedExperiment, please check again")
  
  SummarizedExperiment:::.rbind.SummarizedExperiment(args)
}

# Namrata's thought process ----
# Step 1: created a R and test file using use_r() file and use_test().

# Purpose: combine objects with same samples but different features of interest
# SummarizeExperiment package has rbind function. Is the goal to mimic this functionality?

# Trying to understand rnaseqtools package code ----
# Unname(): removes names / dimnames of the object. Don't we want to keep the colnames per description of
#    SummarizedExperiment::rbind()? "The colnames in rowData(SummarizedExperiment) must match or an error is thrown. 
#                                   Duplicate columns of colData(SummarizedExperiment) must contain the same data."

# General question: what does it mean to have a . in function name -- .rbind.SummarizedExperiment(args)?

# How are they able to call .rbind.SummarizedExperiment function from SummarizedExperiment package? 
# -- if we want to call this function, then should it just be SummarizedExperiment::rbind()? what is row 15 doing?

# What is the rationale of adding a SE_check for class in the function vs adding it as a test in test_that?

# To test my function as I go: should I create dummy SE? or should I be using a sample dataset from somewhere?
# -- what should be the workflow here? Test only directly using test_that?

# Roxygen comments -- where will they go in this methods file? All the way at the top of above rbind function?
# -- when I add Roxygen skeleton it to rbind, it gets added right above rbind
# 


library(SummarizedExperiment)
?SummarizedExperiment::`rbind,SummarizedExperiment-method`
nrows <- 200; ncols <- 6
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
                     row.names=LETTERS[1:6])
se0 <- SummarizedExperiment(assays=SimpleList(counts=counts),
                            colData=colData)
se0
dim(se0)
dimnames(se0)
assayNames(se0)
head(assay(se0))
assays(se0) <- endoapply(assays(se0), asinh)
head(assay(se0))

rowData(se0)
colData(se0)

se0[, se0$Treatment == "ChIP"]
subset(se0, select = Treatment == "ChIP")

## rbind() combines objects with the same samples but different
## features of interest:
se1 <- se0
se2 <- se1[1:50,]
rownames(se2) <- letters[seq_len(nrow(se2))]
cmb2 <- rbind(se1, se2)
dim(cmb2)
dimnames(cmb2)
