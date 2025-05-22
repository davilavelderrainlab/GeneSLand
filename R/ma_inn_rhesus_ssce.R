#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using rhesus
#' PFC inhibitory neurons subtypes from Ma et al. (DOI: 10.1126/science.abo7257).
#' Only subtypes in common with the other primates species (human, chimpanzee,
#' marmoset) were considered.
#'
#' @format A SSCE with 50 rows and 15683 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × genes)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from Ma et al. rhesus inhibitory neurons subtypes pseudobulk
"ma_inn_rhesus_ssce"
