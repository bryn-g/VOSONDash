#' @title Get network metrics from an igraph object
#'
#' @description Function creates a list of network metrics for a graph.
#' 
#' @details Default mode is \code{"strong"}.
#' 
#' @param g \pkg{igraph} graph object.
#' @inheritParams igraph::is_connected
#' @param warnings Logical. Suppress any warning messages. Default is \code{"TRUE"}.
#'   
#' @return Network metrics as named list
#'
#' @export
get_graph_metrics <- function(g, mode = "strong", warnings = FALSE) {
  f <- function(x) x
  if (!warnings) f <- function(x) suppressWarnings(x)
  
  directed <- igraph::is_directed(g)
  list(
    "directed" = directed,
    "simple" = igraph::is_simple(g),
    "nodes_n" = igraph::gorder(g),
    "edges_n" = igraph::gsize(g),
    "connected" = igraph::is_connected(g, mode = mode),
    "comps_mode" = ifelse(directed, mode, "ignored (undirected)"),
    "comps_n" = igraph::count_components(g, mode = mode),
    "isos_n" = length(which(igraph::degree(g) == 0)),
    "isos_loops_n" = length(igraph::degree(g, loops = TRUE)),
    "density" = igraph::edge_density(g),
    "ave_geodesic_dist" = igraph::mean_distance(g),
    "global_clust_coeff" = igraph::transitivity(g),
    "reciprocity_def" = igraph::reciprocity(g, mode = "default"),
    "reciprocity_ratio" = igraph::reciprocity(g, mode = "ratio")
  ) |> f()
}

#' @title Get graph centralization measures for an igraph object
#'
#' @description Function creates a list of centralization measures for a graph.
#'
#' @details Default values for loops, normalized and directed are \code{"TRUE"}.
#' 
#' @param g \pkg{igraph} graph object.
#' @inheritParams igraph::degree
#' @inheritParams igraph::centr_betw
#' @param warnings Logical. Suppress any warning messages. Default is \code{"TRUE"}.
#' 
#' @return Graph centralization measures as named list
#'
#' @export
get_graph_centrlzn <- function(g, loops = TRUE, normalized = TRUE, directed = TRUE, warnings = FALSE) {
  f <- function(x) x
  if (!warnings) f <- function(x) suppressWarnings(x)
  
  list(
    "degree" = igraph::centr_degree(g, mode = "total", loops = loops, normalized = normalized)$centralization,
    "indegree" = igraph::centr_degree(g, mode = "in", loops = loops, normalized = normalized)$centralization,
    "outdegree" = igraph::centr_degree(g, mode = "out", loops = loops, normalized = normalized)$centralization,
    "betweenness" = igraph::centr_betw(g, directed = directed, normalized = normalized)$centralization,
    "closeness" = igraph::centr_clo(g, normalized = normalized)$centralization
  ) |> f()
}
