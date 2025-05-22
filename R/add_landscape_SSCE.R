#' Add landscape view
#'
#' @param ssce The specificity SingleCellExperiment object
#' @param addClusters A boolean variable to define if clusters should be computed
#'
#' @return The specificity SingleCellExperiment object with the UMAP coordinates
#' and colors saved in colData
#' (if addClusters is TRUE, also the membership vector saved in 'cluster')
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- initSSCE(profile)
#' view <- add_landscape_SSCE(out)
add_landscape_SSCE <- function(ssce,
                               addClusters=FALSE) {
  G <- ACTIONet::buildNetwork(as.matrix(SingleCellExperiment::counts(ssce)),
                              distance_metric = "l2")
  o <- irlba::irlba(as.matrix(SingleCellExperiment::counts(ssce)),
                    k = 20)
  Coords <- ACTIONet::layoutNetwork(G,
                                    initial_position = o$v,
                                    n_epochs = 500,
                                    spread = 1)
  ssce@colData$x <- Coords$coordinates[,1]
  ssce@colData$y <- Coords$coordinates[,2]
  ssce@colData$col <- grDevices::rgb(Coords$colors)
  if(addClusters) {
    C <- ACTIONet::clusterNetwork(G)
    C[C%in%as.numeric(names(which(table(C)==1)))] <- 0
    ssce@colData$cluster <- C
  }
  return(ssce)
}
