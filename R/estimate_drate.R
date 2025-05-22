#' Estimate the drate
#'
#' @param y The y values
#' @param x The x values (if NULL, it will be from 1 to length of y)
#'
#' @return The drate
#' @export
#'
#' @examples
#' estimate_drate(y = runif(100, 2, 5))
estimate_drate <- function(y,
                                x=NULL) {

  if(is.null(x)) {
    x <- seq(1, length(y))
  }
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]

  delta_x <- diff(x)
  delta_y <- diff(y)
  drates <- delta_y / delta_x

  return(drates)

}
