#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using Human
#' Protein Atlas tissues
#'
#' @format A SSCE with 50 rows and 18848 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × genes)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from HPA tissues
#' @export
hpa_tissue_ssce <- function() {
  filepath <- system.file("extdata", paste0("hpa_tissue_ssce", ".rda"), package = "GeneSLand")
  if (filepath == "") stop("File not found.")
  obj_name <- load(filepath)
  get(obj_name)
}
