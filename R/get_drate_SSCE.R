#' Calculate dRate for a ssce object
#'
#' @param ssce The specificity single cell experiment object
#' @param genes If some genes are specified, the dRate of only those genes
#' will be computed
#' @param add_one If TRUE, to the Scores vector an initial 1 will be added, to
#' homogeneize the starting point of multiple genes in the computation of the
#' dRate.
#'
#' @return The dRate scores as a vector.
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
#' get_drate_SSCE(out)
get_drate_SSCE <- function(ssce,
                           genes=NULL,
                           add_one=FALSE) {

  if(!is.null(genes)) {
    genes <- genes[which(genes %in% colnames(ssce))]
    ssce <- ssce[,match(genes, colnames(ssce))]
  }

  drates <- apply(SingleCellExperiment::counts(ssce),
                  2,
                  estimate_drate_mean,
                  add_one = add_one)

  drates <- drates * (-1)

  return(drates)

}
