set.seed(123)
n_profiles <- 100
n_genes <- 10000
counts <- matrix(data = rpois(n_profiles * n_genes, lambda = 1), ncol = n_genes)
colnames(counts) <- paste0('Cell-', seq(1,dim(counts)[2]))
rownames(counts) <- paste0('Gene-', seq(1,dim(counts)[1]))

test_that("Returns a list", {
  expect_s4_class(get_lb_scores(counts, GeneSet = rownames(counts)[sample(seq(1, nrow(counts)), 100)])
, 'SimpleList')
})

test_that("List of length 7", {
  expect_equal(length(get_lb_scores(counts, GeneSet = rownames(counts)[sample(seq(1, nrow(counts)), 100)]))
               , 7)
})

test_that("List of length 3", {
  expect_equal(length(get_lb_scores(counts, GeneSet = rownames(counts)[sample(seq(1, nrow(counts)), 100)], estRand = F))
               , 3)
})
