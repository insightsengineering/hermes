setGeneric( "HermesData", function(se, filename = deparse(substitute(se)), ...) standardGeneric( "HermesData" ) )

#' SummarizedExperiment to HermesData Function
#' 
#' Converts a SummarizedExperiment dataset to HermesData object with 
#' gene information from IGIS database added (will be added later), but empty QC flag variables
#' 
#' @rdname HermesData
#' @aliases HermesData HermesData.SummarizedExperiment
#' 
#' @param se SummarizedExperiment or RangedSummarizedExperiment object
#'   
#' @details This function expects a \code{\linkS4class{SummarizedExperiment}} or
#'   \code{\linkS4class{RangedSummarizedExperiment}}object as input and return a
#'   \code{HermesData} object with placeholder variables with empty
#'   values will be added to the object for convenience of downstream
#'   processing: \code{LowExpressionFlag} will be added to \code{rowData}, while
#'   \code{LowDepthFlag} and \code{TechnicalFailureFlag} are added to
#'   \code{colData}.
#' 
#' @return HermesData object
#' @import Biobase S4Vectors checkmate
#' 
#' @export
#' 
#' @template author_haochenl
#'   
#' @examples
#' \dontrun{
#' #' load("~/expression_set.rda")
#' se <- makeSummarizedExperimentFromExpressionSet(expression_set)
#' class(se) #check class of se object
#' gse <- HermesData(se)
#' }
#' 
setMethod(
  "HermesData",
  "SummarizedExperiment", 
  function(se){
    
    check_class(se, "SummarizedExperiment")
    
    #Validating inputs
    if (isEmpty(assay(se))) 
      stop("Input SummarizedExperiment object has no assay data, please check input.")
    if (any(dim(colData(se)) == 0)) 
      warning("Note the colData in SummarizedExperiment object is empty, pleace check input data.")
    if (any(dim(rowData(se)) == 0))
      warning("Note rowData in SummarizedExperiment object is empty, pleace check input data.")
    
    # Processing of rowData----- (will do later)
    #geneinfo <- getgeneinfo(rownames(assay(se)))
    
    # Processing of colData-----
    colDF <- DataFrame(SampleID = colnames(assay(se)))
    
    HD <- HermesData(assays = assays(se),
                      colData = cbind(colDF, se@colData),
                      metadata = se@metadata)
    
    return(HD)
  }
)




###############################################################################

#' Converting matrix and data frames to Hermes object
#' 
#' Converts an expression matrix dataset and clinical dataset in data frame to a
#' HermesData object with gene information added (geneinfo may be added later), but empty QC flag
#' variables
#' 
#' @param assays One-row-per-feature and one-column-per-sample data frame or 
#'   matrix with named rows (by transcripts) and columns (by sample ID) 
#'   containing counts of RNA-Seq data. Note that \code{assay} rownames() must
#'   either be NULL or identical to rownames of rowData(), while \code{assay}
#'   colnames() must either be NULL or identical to rownames of colData().
#' @param rowData A data frame of feature data, where each row corresponds to 
#'   one transcript or gene in \code{assays}, and each column represent
#'   metadata about the gene. 
#' @param colData A data frame of phenotype data, where each row corresponds to 
#'   one subject or sample, and each column representing some characteristics of
#'   the sample
#' @param metadata A list containing metadata about this experiment
#'   
#' @details This function expects three data frames or matrices as input, and
#'   return a \code{gSummarizedExperiment} object with placeholder variables
#'   with empty values will be added to the object for convenience of downstream
#'   processing: \code{LowExpressionFlag} will be added to \code{rowData}, while
#'   \code{LowDepthFlag} and \code{TechnicalFailureFlag} are added to
#'   \code{colData}.
#'   
#' @return HermesData object
#' 
#' @import Biobase S4Vectors
#' @export
#' 
#' @template author_haochenl
#'   
#' @examples
#' \dontrun{
#' # Simple Example
#' a <- matrix(seq_len(25), nrow = 5, 
#'             dimnames = list(paste0("GeneID:", seq_len(5)*10), paste("ID", seq_len(5))))
#' c <- data.frame(ID = paste("ID", seq_len(5)), 
#'                 SEX = sample(c("F", "M"), 5, TRUE), 
#'                 AGE = rnorm(5, 50, 10))
#' r <- data.frame(GeneID = paste0("GeneID:", seq_len(5)*10))
#' 
#' gse <- df2gSE(assays = a, rowData = r, colData = c)
#' }
#' 
HermesDataFromMatrix <- function(assays, 
                                 rowData = data.frame(), 
                                 colData = data.frame(), 
                                 metadata = list(MIAME())) {
  
  #Data check and validation
  if (is(assays, "data.frame")) {
    assays.matrix <- as.matrix(assays)
  } else if (is(assays, "matrix")) {
    assays.matrix <- assays
  } else stop("Input assays must be a data frame or matrix of numerical data, please check inputs")
  
  check_class(assays.matrix, "matrix")
  if (!is.numeric(assays.matrix)) stop("Input assays must be a data frame or matrix of numerical data, please check inputs")
  
  check_class(rowData, "data.frame")
  check_class(colData, "data.frame")
  check_class(metadata, "list")
  
  if (any(dim(assays)) == 0 | length(dimnames(assays)) != 2 | any(sapply(dimnames(assays), is.null))) {
    stop("Input assays must be an non-empty data frame or matrix with named rows and columns, please check inputs") 
  }
  
  if (nrow(assays) != nrow(rowData)) {
    stop("Number of rows in assays must equal to the number of rows in rowData, please check inputs")
  }
  
  if (ncol(assays) != nrow(colData)) {
    stop("Number of columns in assays must equal to the number of rows in colData, please check inputs")
  }
  
  # Processing of rowData-----
  geneinfo <- getgeneinfo(rownames(assays.matrix))
  
  # Processing of colData-----
  colDF <- DataFrame(Filename = filename,
                     SampleID = colnames(assays.matrix))
  
  gSE <- HermesData(assays   = list(counts = assays.matrix),
                               rowData  = cbind(geneinfo, DataFrame(rowData)),
                               colData  = cbind(colDF, DataFrame(colData)),
                               metadata = metadata)
  
  return(gSE)
}

