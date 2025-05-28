#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using marmoset
#' PFC inhibitory neurons subtypes from Ma et al. (DOI: 10.1126/science.abo7257).
#' Only subtypes in common with the other primates species (human, chimpanzee,
#' rhesus) were considered.
#'
#' @format A SSCE with 50 rows and 15683 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × genes)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from Ma et al. marmoset inhibitory neurons subtypes pseudobulk
#' @export
ma_inn_marmoset_ssce <- function() {
  filepath <- system.file("extdata", paste0("ma_inn_marmoset_ssce", ".rda"), package = "GeneSLand")
  if (filepath == "") stop("File not found.")
  obj_name <- load(filepath)
  get(obj_name)
}
