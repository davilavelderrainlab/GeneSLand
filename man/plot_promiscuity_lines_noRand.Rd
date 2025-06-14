% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_promiscuity_lines_noRand.R
\name{plot_promiscuity_lines_noRand}
\alias{plot_promiscuity_lines_noRand}
\title{plot_promiscuity_lines_noRand
A function to plot the promiscuity line}
\usage{
plot_promiscuity_lines_noRand(prom_out, addLines = FALSE, ...)
}
\arguments{
\item{prom_out}{The resulting object of \code{get_expression_promiscuity_levels}}

\item{addLines}{A boolean variable to define if the lines dividing the plot
in quadrants have to be plotted or not}

\item{...}{The plot standard parameters}
}
\value{
The plot with promiscuity lines
}
\description{
plot_promiscuity_lines_noRand
A function to plot the promiscuity line
}
\examples{
set.seed(123)
p <- matrix(runif(20000,0,10), ncol = 10)
colnames(p) <- paste0('Column-', seq(1, ncol(p)))
rownames(p) <- paste0('Gene-', seq(1, nrow(p)))
out <- get_lb_scores(xProfiles = p,
GeneSet = rownames(p)[sample(seq(1,nrow(p)), 20)])
plot_promiscuity_lines_noRand(out)
}
