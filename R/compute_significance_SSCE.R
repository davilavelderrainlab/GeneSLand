#' Compute the significance against the random distribution
#'
#' @param ssce A ssce built using gene sets
#' @param add_one If TRUE, to the Scores vector an initial 1 will be added, to
#' homogeneize the starting point of multiple genes in the computation of the
#' dRate.
#'
#' @return The ssce with additional colData columns. `LBSignificance` is a
#' list with the Z-score and P-values of the comparison of the line against the
#' random distribution, both in mean and by bin. `LBScoresSignificance` is
#' instead a list containing a DataFrame of the Z-Scores and P-values of the
#' AUC, dRate and lbSpec comparison against random.
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1),
#' size = 10000000,
#' replace = TRUE),
#' ncol=10)
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
#' compute_significance_SSCE(out)
compute_significance_SSCE <- function(ssce,
                                      add_one=FALSE) {

  scores <- SingleCellExperiment::counts(ssce)

  rscores <- ssce$rscores
  r_lists <- ssce$random_list
  r_list_by_length <- lapply(ssce@metadata$random_scores_by_length, t)

  z_scores_and_pvalues <- lapply(colnames(ssce), function(i) {

    score <- scores[,i]
    rscore <- rscores[[i]]
    r_list <- r_lists[[i]]

    z <- (score - rscore)/apply(r_list, 1, stats::sd)
    z[which(is.infinite(z))] <- NaN
    mean_z <- mean(z, na.rm = TRUE)
    z[which(is.nan(z))] <- 0
    return(S4Vectors::List('Z-score' = mean_z,
                           'P-value' = zscore_to_pval(mean_z),
                           'Z-scoreByBin' = z,
                           'P-valueByBin' = zscore_to_pval(z)))
  })

  names(z_scores_and_pvalues) <- colnames(ssce)

  ssce$LBSignificance <- z_scores_and_pvalues

  r_auc <- lapply(r_list_by_length, function(i) {Matrix::colSums(i[-1,] + i[-nrow(i),])/(2*(dim(i)[1]-1))})
  r_dRate <- lapply(r_list_by_length, function(i) {apply(i,
                                                2,
                                                function(x) {estimate_drate_mean(x,
                                                                                 add_one=add_one) * (-1)})})
  r_lbSpec <- lapply(r_list_by_length, function(i) {apply(i,
                                                 2,
                                                 calculate_gene_lbspec,
                                                 high_expression = FALSE)})
  lengths <- ssce$geneGroupsLength

  scores_significance <- lapply(names(ssce$AUC), function(i) {
    z_auc <- (ssce$AUC[i] - mean(r_auc[[paste0('N', lengths[[i]])]]))/stats::sd(r_auc[[paste0('N', lengths[[i]])]])
    z_dRate <- (ssce$dRate[i] - mean(r_dRate[[paste0('N', lengths[[i]])]]))/stats::sd(r_dRate[[paste0('N', lengths[[i]])]])
    z_lbSpec <- (ssce$lbSpec[i] - mean(r_lbSpec[[paste0('N', lengths[[i]])]]))/stats::sd(r_lbSpec[[paste0('N', lengths[[i]])]])
    names(z_auc) <- NULL
    names(z_dRate) <- NULL
    names(z_lbSpec) <- NULL
    p_auc <- zscore_to_pval(z_auc)
    p_dRate <- zscore_to_pval(z_dRate)
    p_lbSpec <- zscore_to_pval(z_lbSpec)

    sig_df <- S4Vectors::DataFrame('Z' = c(z_auc, z_dRate, z_lbSpec),
                                   'P' = c(p_auc, p_dRate, p_lbSpec))
    rownames(sig_df) <- c('AUC', 'dRate', 'lbSpec')
    return(sig_df)
  })

  names(scores_significance) <- names(ssce$AUC)

  ssce$LBScoresSignificance <- scores_significance

  return(ssce)

}
