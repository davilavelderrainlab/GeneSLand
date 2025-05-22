#' Estimate the mean drate
#'
#' @inheritParams estimate_drate
#' @param add_one If TRUE, to the y vector an initial 1 will be added, to
#' homogeneize the starting point of multiple ys.
#'
#' @return The mean of the drate
#' @export
#'
#' @examples
#' estimate_drate_mean(runif(100, 2, 4))
estimate_drate_mean <- function(y,
                                x=NULL,
                                add_one=FALSE) {

  if(add_one) {
    y <- c(1, y)
  }
  if(is.null(x)) {
    x <- seq(1, length(y))
  }
  drate <- estimate_drate(y)

  idxs_zero <- which(drate == 0)

  if(length(idxs_zero) > 1) {
    drate <- drate[-idxs_zero]
  }

  drate_at_root <- mean(drate)

  return(drate_at_root)

}
