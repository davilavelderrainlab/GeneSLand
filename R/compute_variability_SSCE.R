#' Compute the variability inside the gene set
#'
#' @param ssce A ssce built using gene sets
#' @param add_one If TRUE, to the Scores vector an initial 1 will be added, to
#' homogeneize the starting point of multiple genes in the computation of the
#' dRate.
#'
#' @return The ssce with additional colData columns. `IntraVariability` is the
#' variability across bins. `MeanIntraVariability` is the mean of
#' `IntraVariability` when there is a change (so that the line is not 0).
#' `ScoresVariability` is a list with the sd of the lines of the genes in the
#' gene set.
#' @export
#'
#' @examples
#' #' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1),
#' size = 10000000, replace = TRUE), ncol=10)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' colnames(profile) <- paste0('Column-', seq(1, ncol(profile)))
#' out <- createSSCE(profile,
#' geneGroups = list('A' = rownames(profile)[seq(1,50)],
#' 'B' = rownames(profile)[seq(51,100)],
#' 'C' = rownames(profile)[seq(101,150)],
#' 'D' = rownames(profile)[seq(151,200)],
#' 'E' = rownames(profile)[seq(201,250)],
#' 'F' = rownames(profile)[seq(251,300)],
#' 'G' = rownames(profile)[seq(301,350)],
#' 'H' = rownames(profile)[seq(351,400)],
#' 'I' = rownames(profile)[seq(401,450)],
#' 'L' = rownames(profile)[seq(451,500)],
#' 'M' = rownames(profile)[seq(501,550)]))
#' compute_variability_SSCE(out)
compute_variability_SSCE <- function(ssce,
                                     add_one = FALSE) {

  scores <- SingleCellExperiment::counts(ssce)

  intra_scores <- ssce$intrascores

  by_bin_variability <- lapply(intra_scores, function(i) {apply(i, 1, stats::sd)})

  ssce$IntraVariability <- by_bin_variability

  meanIntraVariability <- lapply(names(by_bin_variability), function(i) {

    score <- scores[,i]
    intra_var <- by_bin_variability[[i]]
    m <- mean(intra_var[which(score != 0)])

  })

  names(meanIntraVariability) <- names(by_bin_variability)

  ssce$MeanIntraVariability <- meanIntraVariability

  baseline_auc <- flux::auc(seq(1,dim(scores)[1]), rep(1,dim(scores)[1]))

  intra_auc <- lapply(intra_scores, function(i) {apply(i,
                                                       2,
                                                       flux::auc,
                                                       x = seq(1, length(i)))/baseline_auc})
  intra_dRate <- lapply(intra_scores, function(i) {apply(i,
                                                         2,
                                                         function(x) {estimate_drate_mean(x,
                                                                                          add_one = add_one) * (-1)})})
  intra_lbSpec <- lapply(intra_scores, function(i) {apply(i,
                                                          2,
                                                          calculate_gene_lbspec,
                                                          high_expression = FALSE)})

  auc_variability <- lapply(intra_auc, stats::sd)
  dRate_variability <- lapply(intra_dRate, stats::sd)
  lbSpec_variability <- lapply(intra_lbSpec, stats::sd)

  variability_df <- S4Vectors::DataFrame(cbind('AUC' = unlist(auc_variability),
                                               'dRate' = unlist(dRate_variability),
                                               'lbSpec' = unlist(lbSpec_variability)))

  ssce$ScoresVariability <- variability_df

  return(ssce)

}
