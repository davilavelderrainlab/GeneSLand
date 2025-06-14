% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_multiple_lines_SSCE.R
\name{plot_multiple_lines_SSCE}
\alias{plot_multiple_lines_SSCE}
\title{Plot the LB lines given a ssce and a vector of genes}
\usage{
plot_multiple_lines_SSCE(
  ssce,
  genes,
  addLines = FALSE,
  Rand = FALSE,
  Intra = FALSE,
  where_zero = FALSE,
  genes_colors = NULL,
  linewidths = NULL,
  ...
)
}
\arguments{
\item{ssce}{The Specificity SingleCellExperiment object.}

\item{genes}{The genes or gene sets to plot (a vector of column
names in the SSCE)}

\item{addLines}{If TRUE, the x and y mid points of the first gene
are shown as lines}

\item{Rand}{If TRUE, the random distribution mean (+ and - the SD) of the
first gene is shown.
Valid only if the SSCE is built using gene sets.}

\item{Intra}{If TRUE, the intra gene set distribution mean + and - the SD
of the first gene is shown. Valid only if the SSCE is built using gene sets.}

\item{where_zero}{If TRUE, a vertical line to show where the line arrives at
zero for the first gene is plotted.}

\item{genes_colors}{The colors of the main lines}

\item{linewidths}{The lines width of the main lines (\code{lwd} in plot)}

\item{...}{Additional graphical parameters.}
}
\value{
A plot of the LB lines
}
\description{
Plot the LB lines given a ssce and a vector of genes
}
\examples{
set.seed(123)
profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
out <- createSSCE(profile)
plot_multiple_lines_SSCE(out, c('Gene-1', 'Gene-2'))
}
