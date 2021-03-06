#' Plot a network of coauthors
#'
#' @param network A data frame given by \code{grab_network}
#' @param size_labels Size of the label names
#'
#' @return a \code{ggplot2} object but prints a plot as a side effect.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(coauthornetwork)
#'
#' final_network <- grab_network('citations?user=amYIKXQAAAAJ&hl=en')
#' plot_coauthors(final_network)
#' }
plot_coauthors <- function(network, size_labels = 5) {
  graph <-
    tidygraph::mutate(
      tidygraph::as_tbl_graph(network[c("author", "coauthors")]),
      closeness = suppressWarnings(tidygraph::centrality_closeness())
    )

  ggraph::ggraph(graph, layout = 'kk') +
    ggraph::geom_edge_link(ggplot2::aes_string(alpha = '..index..', color = as.character('from')), alpha = 1/3, show.legend = FALSE) +
    ggraph::geom_node_point(ggplot2::aes_string(size = 'closeness'), alpha = 1/2, show.legend = FALSE) +
    ggraph::geom_node_text(ggplot2::aes_string(label = 'name'), size = size_labels, repel = TRUE, check_overlap = TRUE) +
    ggplot2::labs(title = paste0("Network of coauthorship of", network$author[1])) +
    ggraph::theme_graph(title_size = 16)
}
