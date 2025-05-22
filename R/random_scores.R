#' Get random scores
#'
#' @param xProfiles The profiles on which to build the SSCE
#' @param geneGroups The geneGroups used to build the SSCE
#' @param nRand The number of randomizations to use
#' @param nExpValues The number of expression level bins used
#'
#' @return A list with the random scores for each unique length in the
#' geneGroups list, named using 'N' plus the length.
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 10000000, replace = TRUE), ncol=10)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' colnames(profile) <- paste0('Column-', seq(1, ncol(profile)))
#' out <- random_scores(profile,
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
random_scores <- function(xProfiles,
                          geneGroups,
                          nRand = 100,
                          nExpValues = 50) {

  unique_length <- unique(unlist(lapply(geneGroups, length)))

  P <- as.matrix(xProfiles)
  Levs <- seq(0, max(P), length.out = nExpValues)

  out <- lapply(unique_length, function(l) {

    L <- do.call(rbind, lapply(seq(1,nRand),
                               function(i) {

                                 Rx <- P[sample(rownames(P),
                                                l),]
                                 RScores <- sapply(Levs,
                                                   function(x) {
                                                     mean(Matrix::rowMeans(Rx>x))
                                                   })
                                 return(RScores)

                               }))

    return(L)

  })

  names(out) <- paste0('N', unique_length)

  return(out)

}
