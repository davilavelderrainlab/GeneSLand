#' Get a specificity score per column of `xProfiles` per gene
#'
#' @param xProfiles The expression profiles
#' @param ssce A Specificity Single Cell Experiment object
#'
#' @return A specificity score matrix
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
#' compute_expression_specificity(profile, out)
compute_expression_specificity <- function(xProfiles,
                                           ssce) {

  ssce_lbspec_rank <- rank(ssce$lbSpec)

  exp_rank <- apply(xProfiles[names(ssce_lbspec_rank),],2,rank)

  spec_score <- (ssce_lbspec_rank*exp_rank)/(nrow(exp_rank)**2)

  return(spec_score)

}
