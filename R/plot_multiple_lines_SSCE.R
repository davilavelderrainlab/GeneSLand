#' Plot the LB lines given a ssce and a vector of genes
#'
#' @param ssce The Specificity SingleCellExperiment object.
#' @param genes The genes or gene sets to plot (a vector of column
#' names in the SSCE)
#' @param addLines If TRUE, the x and y mid points of the first gene
#' are shown as lines
#' @param Rand If TRUE, the random distribution mean (+ and - the SD) of the
#' first gene is shown.
#' Valid only if the SSCE is built using gene sets.
#' @param Intra If TRUE, the intra gene set distribution mean + and - the SD
#' of the first gene is shown. Valid only if the SSCE is built using gene sets.
#' @param where_zero If TRUE, a vertical line to show where the line arrives at
#' zero for the first gene is plotted.
#' @param genes_colors The colors of the main lines
#' @param linewidths The lines width of the main lines (`lwd` in plot)
#' @param ... Additional graphical parameters.
#'
#' @return A plot of the LB lines
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
#' plot_multiple_lines_SSCE(out, c('Gene-1', 'Gene-2'))
plot_multiple_lines_SSCE <- function(ssce,
                                     genes,
                                     addLines=FALSE,
                                     Rand=FALSE,
                                     Intra=FALSE,
                                     where_zero=FALSE,
                                     genes_colors=NULL,
                                     linewidths=NULL,
                                     ...) {

  if(is.null(genes_colors)) {
    genes_colors <- rep('black', length(genes))
  }
  if(is.null(linewidths)) {
    linewidths <- rep(2, length(genes))
  }
  plot_lines_SSCE(ssce,
                  genes[1],
                  addLines,
                  Rand,
                  Intra,
                  where_zero,
                  gene_color = genes_colors[1],
                  linewidth = linewidths[1],
                  ...)
  for(i in seq(2, length(genes))) {
    graphics::lines(SingleCellExperiment::counts(ssce)[,genes[i]],
                    col = genes_colors[i],
                    lwd = linewidths[i])
  }

}
