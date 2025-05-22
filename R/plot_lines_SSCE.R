#' Plot the LB lines given a ssce
#'
#' @param ssce The Specificity SingleCellExperiment object.
#' @param gene The gene or gene set to plot (a column in the SSCE)
#' @param addLines If TRUE, the x and y mid points are shown as lines
#' @param Rand If TRUE, the random distribution mean (+ and - the SD) are shown.
#' Valid only if the SSCE is built using gene sets.
#' @param Intra If TRUE, the intra gene set distribution mean + and - the SD are
#' shown. Valid only if the SSCE is built using gene sets.
#' @param where_zero If TRUE, a vertical line to show where the line arrives at
#' zero is plotted.
#' @param gene_color The color of the main line
#' @param linewidth The line width of the main line (`lwd` in plot)
#' @param ... Additional graphical parameters.
#'
#' @return A plot of the LB line
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
#' plot_lines_SSCE(out, 'Gene-1')
plot_lines_SSCE <- function(ssce,
                            gene,
                            addLines=FALSE,
                            Rand=TRUE,
                            Intra=TRUE,
                            where_zero=FALSE,
                            gene_color='black',
                            linewidth=2,
                            ...) {

  if(length(gene) > 1) {
    stop('More than a gene is given, which will produce an unwanted result')
  }

  score <- SingleCellExperiment::counts(ssce)[,gene]
  plot(score, col = gene_color, type = "l", lwd = linewidth,
       xlab = "expression level (L) (bins)", ylab = "expression breadth (B)",
       ylim = c(0, 1), las = 1, ...)
  if (addLines) {
    graphics::abline(v = length(score)/2, lty = 2)
    graphics::abline(h = 0.5, lty = 2)
  }

  if(!is.null(ssce$rscores) & Rand) {
    rscore <- ssce$rscores[[gene]]
    rlist <- ssce$random_list[[gene]]
    graphics::points(rscore,
                     type = "l",
                     lwd = 2,
                     col = "grey",
                     lty = 2)
    max_score <- rscore + apply(rlist,
                                1,
                                stats::sd)
    max_score[which(max_score > 1)] <- 1
    graphics::points(max_score,
                     type = "l",
                     lwd = 1.5,
                     col = scales::alpha("lightgrey",
                                         0.5),
                     lty = 4)
    min_score <- rscore - apply(rlist,
                                1,
                                stats::sd)
    min_score[which(min_score < 0)] <- 0
    graphics::points(min_score,
                     type = "l",
                     lwd = 1.5,
                     col = scales::alpha("lightgrey",
                                         0.5),
                     lty = 4)
  }

  if(!is.null(ssce$rscores) & Intra) {
    intra_sd <- ssce$intrascores[[gene]]
    max_score <- score + apply(intra_sd,
                               1,
                               stats::sd)
    max_score[which(max_score > 1)] <- 1
    graphics::points(max_score,
                     type = "l",
                     lwd = 1.5,
                     col = scales::alpha("black",
                                         0.7),
                     lty = 3)
    min_score <- score - apply(intra_sd,
                               1,
                               stats::sd)
    min_score[which(min_score < 0)] <- 0
    graphics::points(min_score,
                     type = "l",
                     lwd = 1.5,
                     col = scales::alpha("black",
                                         0.7),
                     lty = 3)
  }

  if(where_zero) {
    graphics::abline(v = min(which(score == 0)), lwd = 2, col = scales::alpha("red",
                                                                              0.5), lty = 2)
  }

}
