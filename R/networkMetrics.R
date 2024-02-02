#' @title Get graph network metrics
#' 
#' @description Function creates a vector of calculated network metrics for a graph. 
#' 
#' @param g \pkg{igraph} graph object.
#' @param component_type Character string. Use strongly or weakly connected components by specifying \code{"strong"} or 
#' \code{"weak"}. Ignored for undirected graphs. Default is \code{"strong"}.
#' 
#' @return Network metrics as named vector.
#' 
#' @export
getNetworkMetrics <- function(g, component_type = "strong") {
  metrics <- list(
    "directed" = igraph::is_directed(g),
    "nodes" = igraph::vcount(g),
    "edges" = igraph::ecount(g),
    "components_type" = ifelse(igraph::is_directed(g), component_type, "not used"),
    "components" = igraph::count_components(g, mode = component_type),
    "isolates" = length(which(igraph::degree(g) == 0)),
    "density" = igraph::edge_density(g),
    "ave_geodesic_dist" = igraph::mean_distance(g),
    "global_clust_coeff" = igraph::transitivity(g),
    "reciprocity_def" = igraph::reciprocity(g, mode = "default"),
    "reciprocity_ratio" = igraph::reciprocity(g, mode = "ratio"),
    "degree" = igraph::centr_degree(g)$centralization,
    "indegree" = igraph::centr_degree(g, mode = "in")$centralization,
    "outdegree" = igraph::centr_degree(g, mode = "out")$centralization,
    "betweenness" = igraph::centr_betw(g)$centralization,
    "closeness" = suppressWarnings(igraph::centr_clo(g)$centralization)
  )
}
