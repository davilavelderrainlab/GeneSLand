#' Add specificity clusters
#'
#' @param ssce The specificity SingleCellExperiment object
#' @param colPal A possible color palette, if wanted
#' @param G An integer vector specifying the numbers of mixture components (clusters)
#' for which the BIC is to be calculated. The default is G=1:9.
#' @param xProfiles The expression profiles
#'
#' @return The specificity SingleCellExperiment object with the profiles and
#' scores per group in metadata, plus a color per group.
#' @importFrom mclust Mclust
#' @importFrom mclust mclustBIC
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- initSSCE(profile)
#' spec <- add_clusters_SSCE(out, xProfiles = profile)
add_clusters_SSCE <- function(ssce,
                                   colPal=NULL,
                                   G = seq(1,9),
                                   xProfiles) {
  C <- Mclust(cbind(ssce$AUC, ssce$dRate, ssce$lbSpec), G = G)
  ssce$specificity_group <- paste0("c",C$classification)
  ssce@metadata$mclustBIC <- C[c("classification","uncertainty","BIC")]

  specificity_group_profiles <- do.call(cbind, lapply(split(as.data.frame(ssce@colData[,c("AUC","dRate","lbSpec","initB")]),
                                                            ssce$specificity_group),
                                                      Matrix::colMeans))

  ssce@metadata$specificity_group_profiles <- scale(t(specificity_group_profiles))
  ssce@metadata$specificity_group_behavior <- do.call(rbind, lapply(splitSSCE(ssce, ssce$specificity_group), function(i) Matrix::rowMeans(SingleCellExperiment::counts(i))))

  if(is.null(colPal)) colPal <- ggpubr::get_palette("npg", length(unique(ssce$specificity_group)))
  x <- sort(unique(ssce$specificity_group))
  xc <- colPal[seq(1, length(x))]
  names(xc) <- x
  ssce$specificity_group_col <- xc[ssce$specificity_group]
  return(ssce)
}
