#' Plot the Specificity SingleCellExperiment object
#'
#' @param ssce The Specificity SingleCellExperiment object
#' @param values The names of the colData parameter to show or vector of values
#' to plot. It has to be numeric. If NULL, the standard UMAP will be plotted.
#' @param size The size of the points. Defaults to 3.
#' @param genes Which, if any (NULL), genes should be displayed in the UMAP.
#' @param ... Additional graphical parameters for geom_label_repel
#'
#' @importFrom rlang .data
#' @return The ggplot of the SSCE
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
#' plotSSCE(out)
plotSSCE <- function(ssce,
                     values=NULL,
                     size=3,
                     genes=NULL,
                     ...) {

  genes <- genes[which(genes %in% colnames(ssce))]

  if(is.character(values) & length(values) == 1) {
    values <- ssce@colData[,values]
  }

  if(!is.null(values)) {
    baseline <- viridis::magma(length(unique(values))+ceiling(length(unique(values))*5/100))
    baseline <- baseline[seq(1, length(unique(values)))]
    names(baseline) <- sort(unique(values))
    col_vec <- baseline[as.character(values)]
    names(col_vec) <- col_vec
  } else {
    col_vec <- ssce$col
    names(col_vec) <- ssce$col
  }

  alpha_vec <- rep(1, length(ssce$x))
  if(!is.null(genes)) {

    idxs <- match(genes, colnames(ssce))

    new_col_vec <- rep(NA, length(col_vec))
    new_col_vec[idxs] <- col_vec[idxs]
    names(new_col_vec) <- names(col_vec)
    col_vec <- new_col_vec

    alpha_vec[which(!seq(1,length(alpha_vec)) %in% idxs)] <- 0.01

    border_colors <- new_col_vec
    border_colors[which(is.na(border_colors))] <- 'lightgrey'

    labels <- rep(NA, length(col_vec))
    sample_idx <- sample(seq(1, length(idxs)), min(length(genes), 50))
    labels[idxs[sample_idx]] <- genes[sample_idx]

    df <- data.frame(x = ssce$x,
                     y = ssce$y,
                     fill = col_vec,
                     alpha = alpha_vec,
                     col = border_colors)

    g <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x,
                                          y = .data$y,
                                          color = .data$col,
                                          alpha = .data$alpha,
                                          fill = .data$fill)) +
      ggplot2::geom_point(size = size,
                          fill = df$fill,
                          color = df$col) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

  } else {
    border_colors <- col_vec

    df <- data.frame(x = ssce$x,
                     y = ssce$y,
                     fill = col_vec,
                     col = border_colors)

    g <-  ggplot2::ggplot(df,  ggplot2::aes(x = .data$x,
                                            y = .data$y,
                                            color = .data$col,
                                            fill = .data$fill)) +
      ggplot2::geom_point(size = size, fill = df$fill, color = df$col) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")
  }


  if(!is.null(genes)) {
    g <-  g + ggrepel::geom_label_repel(ggplot2::aes(label = labels),
                                        fill = '#FFFFFF',
                                        alpha = 0.8,
                                        col = df$fill,
                                        ...)
  }

  return(g)

}
