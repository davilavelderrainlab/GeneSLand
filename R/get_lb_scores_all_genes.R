#' get_lb_scores_all_genes
#' Defines the promiscuity levels of all genes in a given
#' set of profiles `xProfiles`, defined the number of expression values to use
#' as a threshold `n_bins`
#'
#' @param xProfiles The set of profiles to be used
#' @param n_bins The number of expression values to use as threshold to
#' compute the promiscuity (defaults to 50)
#' @param highExpression A boolean to define whether the lbSpec needs to
#' be weighted on high expression or not
#' @param add_one If TRUE, to the Scores vector an initial 1 will be added, to
#' homogeneize the starting point of multiple genes in the computation of the
#' dRate.
#'
#' @return A list containing the promiscuity scores (`scores`),
#' the decreasing rate (`dRate`) and the specificity (`lbSpec`). If a gene set
#' is given, also the scores for each gene in the gene set are given.
#' If estRand=T, also the random promiscuity scores (`rscores`), the z-score
#' (`z`) and p-values (`p`) of the difference with random and the random scores
#' of each iteration (`random_list`).
#'
#' @examples
#' set.seed(123)
#' p <- matrix(runif(20000,0,10), ncol = 10)
#' colnames(p) <- paste0('Column-', seq(1, ncol(p)))
#' rownames(p) <- paste0('Gene-', seq(1, nrow(p)))
#' out <- GeneSLand:::get_lb_scores_all_genes(xProfiles = p)
get_lb_scores_all_genes <- function(xProfiles,
                                    n_bins = 50,
                                    highExpression = FALSE,
                                    add_one = TRUE) {

  genes <- rownames(xProfiles)
  P <- as.matrix(xProfiles)

  genes <- intersect(genes, rownames(P))
  P <- P[genes, , drop = FALSE]

  Levs <- seq(0, max(P), length.out = n_bins)

  Scores <- matrix(
    NA_real_,
    nrow = nrow(P),
    ncol = length(Levs),
    dimnames = list(rownames(P), as.character(Levs))
  )

  for (j in seq_along(Levs)) {
    Scores[, j] <- rowMeans(P > Levs[j])
  }

  dRate <- apply(
    Scores,
    1,
    function(s) estimate_drate_mean(s, add_one = add_one) * (-1)
  )

  lbSpec <- apply(
    Scores,
    1,
    function(s) calculate_gene_lbspec(s, high_expression = highExpression)
  )

  dRate[is.nan(dRate)] <- 0
  lbSpec[is.nan(lbSpec)] <- 0

  colnames(Scores) <- NULL
  names(dRate) <- NULL
  names(lbSpec) <- NULL
  o <- lapply(seq_len(nrow(Scores)), function(i) {
    S4Vectors::List(
      scores = Scores[i, ],
      dRate = dRate[i],
      lbSpec = lbSpec[i]
    )
  })

  names(o) <- rownames(Scores)

  return(o)
}
