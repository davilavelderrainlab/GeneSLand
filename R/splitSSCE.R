#' Split the Specificity SingleCellExperiment object based on a membership vector
#'
#' @param ssce The specificity SingleCellExperiment object
#' @param colVec The membership vector
#'
#' @return The splitted ssce
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- initSSCE(profile)
#' splitSSCE <- splitSSCE(out, colVec = c(rep('A', 3), rep('B', 2), rep('C', 5)))
splitSSCE <- function(ssce, colVec) {
  groups <- as.character(sort(unique(colVec)))
  temp.ssce.split <- lapply(groups, function(i) ssce[,colVec==i])
  names(temp.ssce.split) <- groups
  return(temp.ssce.split)
}
