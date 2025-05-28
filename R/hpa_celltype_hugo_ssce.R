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
#' @export
hpa_celltype_hugo_ssce <- function() {
  filepath <- system.file("extdata", paste0("hpa_celltype_hugo_ssce", ".rda"), package = "GeneSLand")
  if (filepath == "") stop("File not found.")
  obj_name <- load(filepath)
  get(obj_name)
}

