#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using marmoset
#' PFC glia subtypes from Ma et al. (DOI: 10.1126/science.abo7257).
#' Only subtypes in common with the other primates species (human, chimpanzee,
#' rhesus) were considered.
#'
#' @format A SSCE with 50 rows and 15702 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × genes)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from Ma et al. marmoset glia subtypes pseudobulk
"ma_glia_marmoset_ssce"
