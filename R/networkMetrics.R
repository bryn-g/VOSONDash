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
  metrics <- list()
  
  metrics['directed'] <- ifelse(igraph::is.directed(g), TRUE, FALSE)
  metrics['nodes'] <- igraph::vcount(g)
  metrics['edges'] <- igraph::ecount(g)
  metrics['components_type'] <- ifelse(metrics['directed'], component_type, "not used")
  metrics['components'] <- count_components(g, mode = component_type)
  metrics['isolates'] <- length(which(igraph::degree(g) == 0))
  metrics['density'] <- igraph::graph.density(g)
  metrics['ave_geodesic_dist'] <- igraph::mean_distance(g)
  metrics['global_clust_coeff'] <- igraph::transitivity(g)
  metrics['reciprocity_def'] <- igraph::reciprocity(g, mode = "default")
  metrics['reciprocity_ratio'] <- igraph::reciprocity(g, mode = "ratio")
  metrics['degree'] <- igraph::centr_degree(g)$centralization
  metrics['indegree'] <- igraph::centr_degree(g, mode = "in")$centralization
  metrics['outdegree'] <- igraph::centr_degree(g, mode = "out")$centralization
  metrics['betweenness'] <- igraph::centr_betw(g)$centralization
  metrics['closeness'] <- suppressWarnings(igraph::centr_clo(g)$centralization)

  metrics  
}
