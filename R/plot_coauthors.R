#' Plot a network of coauthors
#'
#' @param network A data frame given by \code{grab_network}
#'
#' @return a \code{ggplot2} object but prints a plot as a side effect.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' final_network <- grab_network('citations?user=amYIKXQAAAAJ&hl=en')
#' plot_coauthors(final_network)
#' }
plot_coauthors <- function(network) {
  graph <-
    network[c("author", "coauthors")] %>%
    tidygraph::as_tbl_graph() %>%
    tidygraph::mutate(closeness = suppressWarnings(tidygraph::centrality_closeness()))

  graph %>%
    ggraph::ggraph(layout = 'kk') +
    ggraph::geom_edge_link(ggplot2::aes_string(alpha = '..index..', color = as.character('from')), alpha = 1/3, show.legend = FALSE) +
    ggraph::geom_node_point(ggplot2::aes_string(size = 'closeness'), alpha = 1/2, show.legend = FALSE) +
    ggraph::geom_node_text(ggplot2::aes_string(label = 'name'), size = 5, repel = TRUE, check_overlap = TRUE) +
    ggplot2::labs(title = glue::glue("Network of coauthorship of {network$author[1]}")) +
    ggraph::theme_graph(title_size = 16)
}
