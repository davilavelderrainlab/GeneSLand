#' Calculate lbspec
#'
#' @param promiscuity The promiscuity scores obtained from
#' `get_expression_promiscuity_levels`
#' @param high_expression A boolean variable to prioritize genes that have a higher
#' expression in specific tissues compared to the rest
#'
#' @return A lbspec score per gene
#' @export
#'
#' @examples
#' set.seed(123)
#' p <- matrix(runif(20000,0,10), ncol = 10)
#' colnames(p) <- paste0('Column-', seq(1, ncol(p)))
#' rownames(p) <- paste0('Gene-', seq(1, nrow(p)))
#' out <- get_lb_scores(xProfiles = p,
#' GeneSet = rownames(p)[sample(seq(1,nrow(p)), 20)])
#' calculate_lbspec(out)
calculate_lbspec <- function(promiscuity,
                             high_expression = FALSE) {

  if('scores' %in% names(promiscuity)) {
    promiscuity <- list(promiscuity)
  }

  scores <- unlist(lapply(promiscuity, function(i) {

    ys <- i$scores
    xs <- seq(1, length(ys))

    no_zero_ys <- ys[which(ys != 0)]

    if(high_expression) {
      norm <- length(ys)
    } else {
      norm <- length(no_zero_ys)
    }

    delta_x <- c()
    for(y in unique(ys)) {

      dx <- length(no_zero_ys[which(no_zero_ys <= y)])
      delta_x <- c(delta_x, dx)

    }

    spec_scores <- c()
    for(n in seq(1, length(delta_x)-1)) {

      sel_y <- unique(ys)[n]
      sel_delta_x <- delta_x[n]

      delta_y <- 1 - sel_y

      lbspec_score <- (delta_y * sel_delta_x/norm)**(1/2)
      spec_scores <- c(spec_scores, lbspec_score)

    }

    return(mean(spec_scores))

  }))

  return(scores)

}
