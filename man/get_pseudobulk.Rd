% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pseudobulk.R
\name{get_pseudobulk}
\alias{get_pseudobulk}
\title{Get pseudobulk counts from a single cell expression matrix}
\usage{
get_pseudobulk(expression_matrix, membership_vector)
}
\arguments{
\item{expression_matrix}{The single cell counts}

\item{membership_vector}{The membership vector with groups for each cell}
}
\value{
The summed counts and the CPMs as an S4Vectors List
}
\description{
Get pseudobulk counts from a single cell expression matrix
}
\examples{
set.seed(123)
sc <- matrix(sample(seq(1, 1000, by = 0.1), size = 10000, replace = TRUE), ncol=1000)
rownames(sc) <- paste0('Gene-', seq(1, nrow(sc)))
colnames(sc) <- paste0('Column-', seq(1, ncol(sc)))
mv <- sample(LETTERS, ncol(sc), replace = TRUE)
get_pseudobulk(sc, mv)
}
