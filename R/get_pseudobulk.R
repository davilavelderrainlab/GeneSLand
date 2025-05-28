#' Get pseudobulk counts from a single cell expression matrix
#'
#' @param expression_matrix The single cell counts
#' @param membership_vector The membership vector with groups for each cell
#'
#' @return The summed counts and the CPMs as an S4Vectors List
#' @export
#'
#' @examples
#' set.seed(123)
#' sc <- matrix(sample(seq(1, 1000, by = 0.1), size = 10000, replace = TRUE), ncol=1000)
#' rownames(sc) <- paste0('Gene-', seq(1, nrow(sc)))
#' colnames(sc) <- paste0('Column-', seq(1, ncol(sc)))
#' mv <- sample(LETTERS, ncol(sc), replace = TRUE)
#' get_pseudobulk(sc, mv)
get_pseudobulk <- function(expression_matrix,
                           membership_vector) {

  Ugroups <- sort(names(which(table(membership_vector)>1)))
  pseudobulk <- do.call(cbind,
                 lapply(Ugroups,
                        function(i) {Matrix::rowSums(methods::as(expression_matrix[,membership_vector==i],
                                                        "sparseMatrix"))}))
  colnames(pseudobulk) <- Ugroups
  rownames(pseudobulk) <- rownames(expression_matrix)

  CPM <- log2(t(t(pseudobulk)/Matrix::colSums(pseudobulk, na.rm = TRUE))*1000000+1)

  pseudobulk_list <- S4Vectors::List(counts=pseudobulk, CPM=CPM)

  return(pseudobulk_list)

}
