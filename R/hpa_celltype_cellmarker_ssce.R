#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using Human
#' Protein Atlas celltypes and CellMarker 2.0 gene groups
#'
#' @format A SSCE with 50 rows and 1019 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × gene groups)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from HPA celltypes and CellMarker 2.0 gene groups
#' @export
hpa_celltype_cellmarker_ssce <- function() {
  filepath <- system.file("extdata", paste0("hpa_celltype_cellmarker_ssce", ".rda"), package = "GeneSLand")
  if (filepath == "") stop("File not found.")
  obj_name <- load(filepath)
  get(obj_name)
}

