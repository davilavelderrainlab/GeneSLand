#' Calculate lbSpec for a ssce object
#'
#' @param ssce The specificity single cell experiment object
#' @param high_expression A boolean that defines if high expression is favored
#' in the computation of lbSpec. Defaults to FALSE.
#' @param genes If some genes are specified, the lbspec of only those genes
#' will be computed
#'
#' @return The lbSpec scores as a vector.
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
#' get_lbspec_SSCE(out)
get_lbspec_SSCE <- function(ssce,
                            genes=NULL,
                            high_expression = FALSE) {

  if(!is.null(genes)) {
    genes <- genes[which(genes %in% colnames(ssce))]
    ssce <- ssce[,match(genes, colnames(ssce))]
  }

  scores <- apply(SingleCellExperiment::counts(ssce), 2, function(i) {

    ys <- i
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

  })

  return(scores)

}
