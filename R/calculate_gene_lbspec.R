#' Calculate lbspec for a single gene
#'
#' @param scores The gene expression breadth
#' @param high_expression A boolean variable to prioritize genes that have a higher
#' expression in specific tissues compared to the rest
#'
#' @return A lbspec score
#' @export
#'
#' @examples
#' set.seed(123)
#' p <- matrix(runif(20000,0,10), ncol = 10)
#' colnames(p) <- paste0('Column-', seq(1, ncol(p)))
#' rownames(p) <- paste0('Gene-', seq(1, nrow(p)))
#' P <- as.matrix(p)
#' Pc <- as.matrix(P[intersect(rownames(p),rownames(P)),])
#' Levs <- seq(0, max(P), length.out=50)
#' Scores <- sapply(Levs, function(i) {mean(rowMeans(Pc>i))})
#' calculate_gene_lbspec(Scores)
calculate_gene_lbspec <- function(scores,
                                  high_expression = FALSE) {

  ys <- scores
  xs <- seq(1, length(ys))

  no_zero_ys <- ys[which(ys != 0)]

  if(high_expression) {
    norm <- length(ys)
  } else {
    norm <- length(no_zero_ys)
  }

  delta_x <- c()
  for(y in unique(ys)) {

    dx <- length(no_zero_ys[which(no_zero_ys <= y)])
    delta_x <- c(delta_x, dx)

  }

  spec_scores <- c()
  for(n in seq(1, length(delta_x)-1)) {

    sel_y <- unique(ys)[n]
    sel_delta_x <- delta_x[n]

    delta_y <- 1 - sel_y

    lbspec_score <- (delta_y * sel_delta_x/norm)**(1/2)
    spec_scores <- c(spec_scores, lbspec_score)

  }

  return(mean(spec_scores))

}
