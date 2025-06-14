#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using Human
#' Protein Atlas celltypes and Gene Ontology Biological Processes
#'
#' @format A SSCE with 50 rows and 5368 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × gene groups)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from HPA celltypes and Gene Ontology Biological Processes
#' @export
hpa_celltype_gobp_ssce <- function() {
  filepath <- system.file("extdata", paste0("hpa_celltype_gobp_ssce", ".rda"), package = "GeneSLand")
  if (filepath == "") stop("File not found.")
  obj_name <- load(filepath)
  get(obj_name)
}

