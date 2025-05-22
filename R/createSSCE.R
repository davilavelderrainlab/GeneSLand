#' Create the Specificity SingleCellExperiment object
#'
#' @param xProfiles The expression profiles
#' @param geneGroups The gene groups to group the profiles
#' @param addClusters A boolean variable to define if clusters should be
#' computed
#' @param compute_significance A boolean to define if the significance of the
#' comparison with random should be computed
#' @param compute_variability A boolean to define if the intra-gene group
#' variability has to be computed
#' @param nRand The number of times the randomization is run (needed when using
#' geneGroups)
#' @param add_one If TRUE, to the Scores vector an initial 1 will be added, to
#' homogeneize the starting point of multiple genes in the computation of the
#' dRate.
#'
#' @return The Specificity SingleCellExperiment object, with AUC, dRate, lbSpec
#' and initB values for each gene, containing as counts the Breadth values for
#' the 50 bins of expression level. It will also contain the x and y coordinates
#' for the UMAP plot, as well as the colors. If addClusters is TRUE, also the
#' clusters. If compute_significance is TRUE and the SSCE is built using gene
#' groups, also the variability intra gene group is computed. If
#' compute_significance and the SSCE is built using gene groups, also the
#' significance against random is computed.
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
createSSCE <- function(xProfiles,
                       geneGroups=NULL,
                       addClusters=FALSE,
                       compute_significance=FALSE,
                       compute_variability=FALSE,
                       nRand=100,
                       add_one=FALSE) {

  xProfiles <- filter_zero_rows(xProfiles)
  ssce <- initSSCE(xProfiles = xProfiles,
                   geneGroups = geneGroups,
                   nRand = nRand,
                   add_one = add_one)
  ssce <- add_landscape_SSCE(ssce = ssce,
                             addClusters = addClusters)
  if(addClusters) {
    ssce <- add_clusters_SSCE(ssce = ssce,
                                   xProfiles = xProfiles)
  }

  if(!is.null(geneGroups) & compute_variability) {
    ssce <- compute_variability_SSCE(ssce)
  }

  if(!is.null(geneGroups) & compute_significance) {
    ssce <- compute_significance_SSCE(ssce,
                                      add_one = add_one)
  }

  return(ssce)

}
