#' zscore_to_pval
#' Converts Z-scores to P-values
#' @param z Z-score
#'
#' @return The p-value
#' @export
#'
#' @examples
#' zscore_to_pval(1.96)
zscore_to_pval <- function(z) {
  p_value <- 2 * stats::pnorm(-abs(z))
  return(p_value)
}
