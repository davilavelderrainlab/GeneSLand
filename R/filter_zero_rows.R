#' Filter zero rows
#'
#' @param m A matrix
#'
#' @return The matrix without rows that sum to 0
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' sub_profile <- filter_zero_rows(profile)
filter_zero_rows <- function(m) {
  m <- m[which(rowSums(m) != 0),]
}
