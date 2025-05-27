#' Title
#'
#' @param new_profiles The whole expression matrix of the genes that you want to
#' map (including the genes to be mapped, but not exclusively).
#' @param ssce The ssce on which to map
#' @param genes The genes to map. It can either be a vector, and then each
#' single gene in the vector will be mapped independently, or a named list,
#' associating genes to a group, in which case the mapped points will be the
#' groups.
#' @param size Size of the dots in the plot
#' @param values Which score to show in the plot as a color gradient
#' @param gene_name If a single gene is mapped, its name can be defined here, as
#' the new_profiles will likely be a vector.
#' @param size_converter How many times bigger the mapped points are with
#' respect to the baseline size
#' @param add_one If TRUE, to the Scores vector an initial 1 will be added, to
#' homogeneize the starting point of multiple genes in the computation of the
#' dRate.
#' @param ... Additional graphical parameters for geom_label_repel
#'
#' @importFrom rlang .data
#' @return A list containing the result of `get_expression_promiscuity_levels`
#' on the new gene(group)s, so their Breadth values, the AUC, dRate and lbSpec
#' scores, plus the plot with the mapped points and the coordinates dataframe.
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- createSSCE(profile)
#' mapped_out <- mapSSCE(profile[2,], ssce = out, genes = 'Gene-1')
mapSSCE <- function(new_profiles,
                    ssce,
                    genes,
                    size = 3,
                    values = 'AUC',
                    gene_name = 'MappedGene',
                    size_converter=1.5,
                    add_one = FALSE,
                    ...) {

  if(is.list(genes)) {
    genes <- lapply(genes, function(i) {i[which(i %in% rownames(new_profiles))]})
    genes <- genes[which(unlist(lapply(genes, length)) > 1)]
    new_counts <- lapply(genes, function(i) {get_lb_scores(new_profiles,
                                                           GeneSet = i,
                                                           estRand = FALSE,
                                                           add_one = add_one)})
    names(new_counts) <- names(genes)
    new_scores <- do.call(cbind, lapply(new_counts, function(i) {i$scores}))

  } else {
    new_counts <- lapply(genes,
                         get_lb_scores,
                         xProfiles = new_profiles,
                         estRand = FALSE,
                         add_one = add_one)
    names(new_counts) <- genes
    new_scores <- do.call(cbind, lapply(new_counts, function(i) {i$scores}))

  }

  new_counts <- lapply(new_counts, function(i) {
    m <- as.matrix(i$scores)
    i$AUC <- sum(m[-1,] + m[-nrow(m),])/(2*(dim(m)[1]-1))
    return(i)
  })

  counts_m <- t(as.matrix(SingleCellExperiment::counts(ssce)))
  colnames(counts_m) <- paste0('Bin', seq(1, ncol(counts_m)))

  target_1 <- ssce$x
  target_2 <- ssce$y

  df_1 <- data.frame(counts_m, target_1)
  df_2 <- data.frame(counts_m, target_2)

  model_1 <- ranger::ranger(target_1 ~ ., data = df_1)
  model_2 <- ranger::ranger(target_2 ~ ., data = df_2)

  new_scores <- as.data.frame(t(new_scores))
  colnames(new_scores) <- colnames(counts_m)

  predicted_x <- stats::predict(model_1, data = new_scores, type = 'response')$predictions
  predicted_y <- stats::predict(model_2, data = new_scores, type = 'response')$predictions

  g <- plotSSCE(ssce, size = size, values = values)

  points_df <- data.frame(x = predicted_x,
                          y = predicted_y,
                          label = names(new_counts),
                          color = rep('tomato', length(new_counts)))

  g <- g +
    ggplot2::geom_point(data = points_df, ggplot2::aes(x = .data$x,
                                                       y = .data$y),
               fill = points_df$color,
               size = size*size_converter,
               color = 'black',
               shape = 21,
               stroke = 0.35)  +
    ggrepel::geom_label_repel(data = points_df, ggplot2::aes(x = .data$x,
                                                             y = .data$y,
                                                             label = .data$label),
                     color = "black",
                     size = 3,
                     inherit.aes = FALSE,
                     box.padding = 0.5,
                     ...)

  return(S4Vectors::List('LineObject' = new_counts,
                         'Plot' = g,
                         'Coordinates' = points_df))

}
