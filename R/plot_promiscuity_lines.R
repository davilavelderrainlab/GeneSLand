#' plot_promiscuity_lines
#' A function to plot the promiscuity scores obtained using
#' `get_expression_promiscuity_levels`
#'
#' @param prom_out The resulting object of `get_expression_promiscuity_levels`
#' @param addLines A boolean variable to define if the lines dividing the plot
#' in quadrants have to be plotted or not
#' @param Rand A boolean variable to define if the random scores lines (mean and
#' stats::sd) have to be plotted
#' @param ... The plot standard parameters
#'
#' @return The plot with promiscuity lines
#' @export
#'
#' @examples
#' set.seed(123)
#' p <- matrix(runif(20000,0,10), ncol = 10)
#' colnames(p) <- paste0('Column-', seq(1, ncol(p)))
#' rownames(p) <- paste0('Gene-', seq(1, nrow(p)))
#' out <- get_lb_scores(xProfiles = p,
#' GeneSet = rownames(p)[sample(seq(1,nrow(p)), 20)])
#' plot_promiscuity_lines(out)
plot_promiscuity_lines <- function(prom_out,
                                   addLines=FALSE,
                                   Rand=TRUE,
                                   ...) {

  plot_promiscuity_lines_noRand(prom_out,
                                addLines = addLines)

  if(Rand) {
    graphics::points(prom_out$rscores,
           type="l",
           lwd=2,
           col='grey',
           lty=2)

    graphics::points(prom_out$rscores+apply(prom_out$random_list, 2, stats::sd),
           type="l",
           lwd=2,
           col=scales::alpha("lightgrey", 0.5),
           lty=2)

    graphics::points(prom_out$rscores-apply(prom_out$random_list, 2, stats::sd),
           type="l",
           lwd=2,
           col=scales::alpha("lightgrey", 0.5),
           lty=2)

  }

}
