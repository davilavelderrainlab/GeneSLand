#' Initialize the Specificity SingleCellExperiment object
#'
#' @param xProfiles The starting profiles
#' @param geneGroups The gene groups to group the profiles
#' @param nRand The number of times the randomization is run (needed when using
#' geneGroups)
#' @param add_one If TRUE, to the Scores vector an initial 1 will be added, to
#' homogeneize the starting point of multiple genes in the computation of the
#' dRate.
#'
#' @return The Specificity SingleCellExperiment object, with AUC, dRate, lbSpec
#' and initB values for each gene, containing as counts the Breadth values for
#' the 50 bins of expression level. If the SSCE is built using geneGroups, it
#' will also return the random mean score in rscores, the random scores list in
#' random_list and the scores for each gene in the gene set in intrascores.
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- initSSCE(profile)
initSSCE <- function(xProfiles,
                    geneGroups=NULL,
                    nRand=100,
                    add_one=FALSE) {
  if(!is.null(geneGroups)) {
    geneGroups <- lapply(geneGroups, function(i) {unique(i[which(i %in% rownames(xProfiles))])})
    geneGroups <- geneGroups[sapply(geneGroups, length)>1]
    random_list <- random_scores(xProfiles, geneGroups, nRand = nRand)
    o <- lapply(geneGroups, function(i) {
      return(get_lb_scores(xProfiles = xProfiles,
                           GeneSet = i,
                           random_list = random_list,
                           add_one=add_one))
    })
    names(o) <- names(geneGroups)

  } else {
    o <- lapply(rownames(xProfiles), function(i) {
      return(get_lb_scores(xProfiles = xProfiles,
                           GeneSet = i,
                           estRand = FALSE,
                           add_one=add_one))
    })
    names(o) <- rownames(xProfiles)
  }

  unique_genes <- unique(unlist(geneGroups))
  intrascores <- do.call(cbind, lapply(unique_genes, function(i) {
     computeLB(xProfiles, GeneSet = i)
  }))
  colnames(intrascores) <- unique_genes

  full_intrascores <- lapply(geneGroups, function(i) {intrascores[,i]})

  Scores <- methods::as(do.call(cbind, lapply(o, function(i) i$scores)), "sparseMatrix")

  AUCs <- Matrix::colSums(Scores[-1,] + Scores[-nrow(Scores),])/(2*(dim(Scores)[1]-1))

  ssce <- SingleCellExperiment::SingleCellExperiment(assays=list(counts=Scores),
                                                     colData = S4Vectors::DataFrame(
                                                       AUC=AUCs,
                                                       dRate=sapply(o, function(i) i$dRate),
                                                       lbSpec=sapply(o, function(i) i$lbSpec)))

  ssce$initB <- SingleCellExperiment::counts(ssce)[1,]

  if(!is.null(geneGroups)) {
    ssce$random_list <- lapply(o, function(i) t(i$random_list))
    ssce$rscores <- lapply(o, function(i) i$rscores)
    ssce$intrascores <- full_intrascores
    ssce@metadata$random_scores_by_length <- random_list
    ssce$geneGroupsLength <- lapply(geneGroups, length)
  }

  return(ssce)

}
