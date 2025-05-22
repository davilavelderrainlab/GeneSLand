#' Specificity Single Cell Experiment object
#'
#' It contains a Specificity Single Cell Experiment object built using rhesus
#' PFC neuronal subtypes from Ma et al. (DOI: 10.1126/science.abo7257). Only
#' subtypes in common with the other primates species (human, chimpanzee,
#' marmoset) were considered.
#'
#' @format A SSCE with 50 rows and 15648 columns:
#' \describe{
#'   \item{assays}{A list containing one matrix named "counts" (expression bins × genes)}
#'   \item{colData}{A `DataFrame` with gene-level information (scores, coordinates)}
#' }
#'
#' @source Processed from Ma et al. rhesus neuron subtypes pseudobulk
"ma_neu_rhesus_ssce"
