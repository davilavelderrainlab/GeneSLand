## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(GeneSLand)

## ----style, echo = FALSE, results = 'asis'------------------------------------
library(BiocStyle)

## ----echo = FALSE-------------------------------------------------------------
library(knitr)

## -----------------------------------------------------------------------------
set.seed(123)
n_profiles <- 50
n_genes <- 5000
counts <- matrix(runif(n_genes*n_profiles,1,10), ncol = n_profiles)
colnames(counts) <- paste0('Cell-', seq(1,dim(counts)[2]))
rownames(counts) <- paste0('Gene-', seq(1,dim(counts)[1]))

## ----results='hide', message=FALSE--------------------------------------------
ssce <- createSSCE(xProfiles = counts)

## -----------------------------------------------------------------------------
plotSSCE(ssce)
# AUC
plotSSCE(ssce, values = 'AUC')
# dRate
plotSSCE(ssce, values = 'dRate')
# lbSpec
plotSSCE(ssce, values = 'lbSpec')

## -----------------------------------------------------------------------------
plot_lines_SSCE(ssce, gene = 'Gene-1')
plot_multiple_lines_SSCE(ssce, genes = c('Gene-1', 'Gene-2'))

## -----------------------------------------------------------------------------
gene_groups <- split(rownames(counts), sample(LETTERS, nrow(counts), replace = TRUE))
ssce_groups <- createSSCE(counts, geneGroups = gene_groups)

## -----------------------------------------------------------------------------
ssce_groups <- compute_significance_SSCE(ssce_groups)
ssce_groups <- compute_variability_SSCE(ssce_groups)

## -----------------------------------------------------------------------------
plot_lines_SSCE(ssce_groups, gene = 'Z')
plot_multiple_lines_SSCE(ssce_groups, genes = c('Z', 'A'))

## -----------------------------------------------------------------------------
sessionInfo()

