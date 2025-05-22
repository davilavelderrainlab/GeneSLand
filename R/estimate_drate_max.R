#' Estimate the maximum of the drate
#'
#' @inheritParams estimate_drate
#'
#' @return The value of the maximum drate
#' @export
#'
#' @examples
#' estimate_drate_max(runif(100, 2, 4))
estimate_drate_max <- function(y,
                               x=NULL) {

  if(is.null(x)) {
    x <- seq(1, length(y))
  }

  drate <- estimate_drate(y)

  drate_at_root <- drate[which.max(abs(drate))]

  return(drate_at_root)

}
