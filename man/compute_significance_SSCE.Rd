% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_significance_SSCE.R
\name{compute_significance_SSCE}
\alias{compute_significance_SSCE}
\title{Compute the significance against the random distribution}
\usage{
compute_significance_SSCE(ssce, add_one = FALSE)
}
\arguments{
\item{ssce}{A ssce built using gene sets}

\item{add_one}{If TRUE, to the Scores vector an initial 1 will be added, to
homogeneize the starting point of multiple genes in the computation of the
dRate.}
}
\value{
The ssce with additional colData columns. \code{LBSignificance} is a
list with the Z-score and P-values of the comparison of the line against the
random distribution, both in mean and by bin. \code{LBScoresSignificance} is
instead a list containing a DataFrame of the Z-Scores and P-values of the
AUC, dRate and lbSpec comparison against random.
}
\description{
Compute the significance against the random distribution
}
\examples{
set.seed(123)
profile <- matrix(sample(seq(1, 100, by = 0.1),
size = 10000000,
replace = TRUE),
ncol=10)
rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
colnames(profile) <- paste0('Column-', seq(1, ncol(profile)))
out <- createSSCE(profile,
geneGroups = list('A' = rownames(profile)[seq(1,50)],
'B' = rownames(profile)[seq(51,100)],
'C' = rownames(profile)[seq(101,150)],
'D' = rownames(profile)[seq(151,200)],
'E' = rownames(profile)[seq(201,250)],
'F' = rownames(profile)[seq(251,300)],
'G' = rownames(profile)[seq(301,350)],
'H' = rownames(profile)[seq(351,400)],
'I' = rownames(profile)[seq(401,450)],
'L' = rownames(profile)[seq(451,500)],
'M' = rownames(profile)[seq(501,550)]))
compute_significance_SSCE(out)
}
