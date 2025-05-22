#' Compute the Level-Breadth scores
#'
#' @param xProfiles The expression profiles
#' @param GeneSet The gene or gene set of interest
#' @param nExpValues Number of expression level bins
#'
#' @return The L-B scores as a vector of B scores at increasing values of L bins
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' computeLB(xProfiles = profile, GeneSet = 'Gene-1')
computeLB <- function(xProfiles,
                      GeneSet,
                      nExpValues = 50) {

  P <- as.matrix(xProfiles)
  Pc <- as.matrix(P[intersect(GeneSet, rownames(P)), ])
  if (length(GeneSet) == 1) {
    Pc <- t(Pc)
    rownames(Pc) <- GeneSet
  }
  Levs <- seq(0, max(P), length.out = nExpValues)
  Scores <- sapply(Levs, function(i) {
    mean(Matrix::rowMeans(Pc > i))
  })
  return(Scores)

}
