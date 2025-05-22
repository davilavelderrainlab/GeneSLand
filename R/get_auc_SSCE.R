#' Calculate AUC for a ssce object
#'
#' @param ssce The specificity single cell experiment object
#' @param genes If some genes are specified, the AUC of only those genes
#' will be computed
#'
#' @return The AUC scores as a vector.
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
#' get_auc_SSCE(out)
get_auc_SSCE <- function(ssce,
                         genes=NULL) {

  if(!is.null(genes)) {
    genes <- genes[which(genes %in% colnames(ssce))]
    ssce <- ssce[,match(genes, colnames(ssce))]
  }

  Scores <- SingleCellExperiment::counts(ssce)

  aucs <- Matrix::colSums(Scores[-1,] + Scores[-nrow(Scores),])/(2*(dim(Scores)[1]-1))

  return(aucs)

}
