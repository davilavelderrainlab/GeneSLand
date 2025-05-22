#' plot_promiscuity_lines_noRand
#' A function to plot the promiscuity line
#' @inheritParams plot_promiscuity_lines
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
#' plot_promiscuity_lines_noRand(out)
plot_promiscuity_lines_noRand <- function(prom_out,
                                          addLines=FALSE,
                                          ...) {

  plot(prom_out$scores,
       col="black",
       type="l",
       lwd=2,
       xlab="expression level (bins)",
       ylab="average number of cell types",
       ylim=c(0,1),
       las=1,
       ...)

  if(addLines) {

    graphics::abline(v=length(prom_out$scores)/2,
           lty=2)

    graphics::abline(h=0.5,
                     lty=2)

    graphics::points(length(prom_out$scores)/2,
           prom_out$xhalf,
           col=2)

    graphics::points(prom_out$yhalf,
           0.5,
           col=2)

  }

}
