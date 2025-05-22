#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using Human
#' Protein Atlas celltypes and HUGO parent terms
#'
#' @format A SSCE with 50 rows and 144 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × gene groups)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from HPA celltypes and HUGO parent terms
"hpa_celltype_hugo_ssce"
