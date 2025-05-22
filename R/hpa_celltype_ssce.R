#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using Human
#' Protein Atlas cell types
#'
#' @format A SSCE with 50 rows and 18782 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × genes)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from HPA celltypes
"hpa_celltype_ssce"
