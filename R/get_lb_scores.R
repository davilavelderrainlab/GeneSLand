#' get_lb_lines_scores
#' Defines the promiscuity levels of a gene or a gene set `GeneSet` in a given
#' set of profiles `xProfiles`, defined the number of expression values to use
#' as a threshold `nExpValues`
#'
#' @param xProfiles The set of profiles to be used
#' @param GeneSet The gene or gene set of interest
#' @param nExpValues The number of expression values to use as threshold to
#' compute the promiscuity (defaults to 50)
#' @param nRand Number of random iterations (defaults to 100)
#' @param estRand A boolean variable to determine if the random baseline has to
#' be computed
#' @param random_list If the GeneSet is of length > 1, you can provide already
#' a list of score matrices obtained by randomization. Each element of the list
#' has to be obtained by using the length of the GeneSet (or GeneSets) and be
#' named using 'N' and the length of the GeneSet (if list of length 1 or
#' GeneSets otherwise).
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
#' @export
#'
#' @examples
#' set.seed(123)
#' p <- matrix(runif(20000,0,10), ncol = 10)
#' colnames(p) <- paste0('Column-', seq(1, ncol(p)))
#' rownames(p) <- paste0('Gene-', seq(1, nrow(p)))
#' out <- get_lb_scores(xProfiles = p,
#' GeneSet = rownames(p)[sample(seq(1,nrow(p)), 20)])
get_lb_scores <- function(xProfiles,
                          GeneSet,
                          nRand=100,
                          nExpValues=50,
                          estRand=TRUE,
                          highExpression=FALSE,
                          random_list=NULL,
                          add_one=FALSE) {

  P <- as.matrix(xProfiles)
  Pc <- as.matrix(P[intersect(GeneSet, rownames(P)), ])
  Levs <- seq(0, max(P), length.out = nExpValues)
  if (length(GeneSet) == 1) {
    Pc <- t(Pc)
    rownames(Pc) <- GeneSet
    Scores <- sapply(Levs, function(i) {
      mean(Matrix::rowMeans(Pc > i))
    })
  } else {
    rownames(Pc) <- GeneSet
    Scores <- sapply(Levs, function(i) {
      mean(Matrix::rowMeans(Pc > i))
    })
  }

  ########################################################################
  if(estRand) {
    if(length(GeneSet) == 1) {

      L <- apply(P, 1, function(i) {

        RScores <- sapply(Levs, function(x) {
          mean(i>x)
        })

      })

      RScores <- Matrix::rowMeans(L)
      random_list <- t(L)

    } else {

      if(is.null(random_list)) {
        L <- lapply(seq(1,nRand),
                    function(i) {

                      Rx <- P[sample(rownames(P),
                                     nrow(Pc)),]
                      RScores <- sapply(Levs,
                                        function(x) {
                                          mean(Matrix::rowMeans(Rx>x))
                                        })
                      return(RScores)

                    })

        RScores <- Matrix::rowMeans(do.call(cbind, L))
        random_list <- do.call(rbind, L)
      } else {
        random_list <- random_list[[paste0('N', nrow(Pc))]]
        RScores <- Matrix::colMeans(random_list)
      }

    }

    z <- (Scores-RScores)/apply(random_list, 2, stats::sd)
    z[is.na(z)] <- 0

  }
  ########################################################################
  drate_mean <- estimate_drate_mean(Scores,
                                    add_one=add_one) * (-1)

  spec <- calculate_gene_lbspec(Scores,
                                high_expression=highExpression)

  if(is.nan(drate_mean)) {
    drate_mean <- 0
  }
  if(is.nan(spec)) {
    spec <- 0
  }

  if(estRand) {
    o <- S4Vectors::List(scores=Scores,
                         dRate=drate_mean,
                         lbSpec=spec,
                         rscores=RScores,
                         random_list=random_list,
                         z=z,
                         p=zscore_to_pval(z))
  } else {
    o <- S4Vectors::List(scores=Scores,
                         dRate=drate_mean,
                         lbSpec=spec)
  }

  return(o)
}
