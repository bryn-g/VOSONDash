#' @title Filter out graph nodes not in component size range
#' 
#' @description This function removes any graph nodes that are in components that fall outside of the specified 
#' component size range.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param mode Character string. Use strongly or weakly connected components by specifying \code{"strong"} or 
#' \code{"weak"}. Ignored for undirected graphs. Default is \code{"strong"}.
#' @param component_range Numeric vector. Min and max values or size range of component.
#' 
#' @return An igraph graph object.
#' 
#' @export
filter_comps <- function(g, mode = "strong", component_range) {
  min_range <- component_range[1]
  max_range <- component_range[2]
  
  graph_clusters <- igraph::components(g, mode = mode)
  
  min_cluster_size <- suppressWarnings(min(graph_clusters$csize)) # suppress no non-missing arguments to min;
  max_cluster_size <- suppressWarnings(max(graph_clusters$csize)) # returning Inf warning
  
  filter_nodes_under <- NULL
  filter_nodes_over <- NULL
  rm_nodes <- c()
  
  # remove nodes not part of components in component size range by name
  if (min_range > min_cluster_size) {
    filter_nodes_under <- names(which(table(graph_clusters$membership) < min_range))
    
    if (length(filter_nodes_under) > 0) {
      rm_nodes <- sapply(filter_nodes_under, function(x) append(rm_nodes, names(which(graph_clusters$membership == x))))
    }
  }
  
  if (max_range < max_cluster_size) {
    filter_nodes_over <- names(which(table(graph_clusters$membership) > max_range))
    
    if (length(filter_nodes_over) > 0) {
      rm_nodes <- sapply(filter_nodes_over, function(x) append(rm_nodes, names(which(graph_clusters$membership == x))))
    }
  }
  
  if (length(rm_nodes) > 0) {
    rm_nodes <- unlist(rm_nodes)
    g <- igraph::delete_vertices(g, rm_nodes)
  }
  
  g
}

#' @title Filter out graph nodes and edges from graph object that are isolates, multi edge or edge loops
#' 
#' @description This function removes multiple edges between nodes and or node edge loops from a 
#' graph.
#' 
#' @note Removing multiple edges or edge loops from a graph will simplify it and remove other edge attributes.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param multi_edges Logical. Include multiple edges between nodes in graph. Default is \code{TRUE}.
#' @param loop_edges Logical. Include node edge loops in graph. Default is \code{TRUE}.
#' 
#' @return An igraph graph object.
#' 
#' @export
filter_edges <- function(g, multi_edges = TRUE, loop_edges = TRUE) {
  
  # remove multiple edges and self loops
  g <- igraph::simplify(g, remove.multiple = multi_edges, remove.loops = loop_edges)
}

# lo1 = layout_with_fr(g)

# i = which(degree(g) == 0)
# g2 = delete.vertices(g, i)
# lo2 = lo1[-i, ]

# g.new <- delete.vertices(g.new, V(g.new)[degree(g.new)==0]) 

#' @title Add centrality measures to graph as node attributes
#' 
#' @description Adds degree, in-degree, out-degree, betweenness and closeness measures to graph as node attributes.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#'
#' @return An igraph graph object.
#' 
#' @export
add_centrality_measures <- function(g) {
  # add degree
  igraph::V(g)$Degree <- igraph::degree(g, mode = "total")
  if (igraph::is_directed(g)) {
    igraph::V(g)$Indegree <- igraph::degree(g, mode = "in")
    igraph::V(g)$Outdegree <- igraph::degree(g, mode = "out")
  } else {
    igraph::V(g)$Indegree <- igraph::V(g)$Outdegree <- 0
  }
  
  # add centrality
  if (igraph::gorder(g) > 1) {
    igraph::V(g)$Betweenness <- as.numeric(sprintf(fmt = "%#.3f", igraph::betweenness(g)))
    # suppress disconnected graph warnings
    igraph::V(g)$Closeness <- as.numeric(sprintf(fmt = "%#.3f", suppressWarnings(igraph::closeness(g))))    
  } else {
    igraph::V(g)$Betweenness <- igraph::V(g)$Closeness <- 0
  }

  g
}

#' @title Get a list of node category attribute names and values
#' 
#' @description This function returns a list of graph node attribute names that match a category attribute prefix 
#' format and their unique values.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param cat_prefix Character string. Category attribute prefix format to match. Default is \code{"vosonCA_"}.
#' 
#' @return A named list of node category attributes and values.
#' 
#' @examples
#' \dontrun{
#' # get a list of voson node categories and values
#' g <- get_pkg_data("DividedTheyBlog_40Alist_release.graphml")
#' 
#' cats <- get_node_cats(g)
#' 
#' # cats
#' # $Stance
#' # [1] "conservative" "liberal"  
#' }
#' 
#' @export
get_node_cats <- function(g, cat_prefix = "vosonCA_") {
  graph_cats <- list()
  
  attr_v <- igraph::vertex_attr_names(g)
  attr_v <- attr_v[grep(paste0("^", cat_prefix), attr_v, perl = TRUE)]
  
  for (voson_attr in attr_v) {
    voson_attr_rm_prefix <- sub(paste0("^", cat_prefix), "", voson_attr)
    graph_cats[[voson_attr_rm_prefix]] <- sort(unique(igraph::vertex_attr(g, voson_attr)))
  }
  
  graph_cats
}

#' @title Check if graph object has text attributes
#' 
#' @description This function checks if a graph has either node or edge text attributes.
#' 
#' @note Uses the \code{VOSON} node and edge text attribute prefix \code{"vosonTxt_"} to determine if attributes are
#' text attributes.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' 
#' @return Result as logical.
#' 
#' @keywords internal
#' @export
has_voson_txt_attr <- function(g) {
  attr_v <- igraph::vertex_attr_names(g)
  attr_v <- attr_v[grep("^vosonTxt_", attr_v, perl = TRUE)]
  
  attr_e <- edge_attr_names(g)
  attr_e <- attr_e[grep("^vosonTxt_", attr_e, perl = TRUE)]
  
  if (length(attr_v) > 0 | length(attr_e) > 0) return(TRUE)
  
  FALSE
}

#' @title Filter out graph nodes not in selected category
#' 
#' @description This function removes nodes that are not in the selected categories values list or sub-categories.   
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param selected_cat Character string. Selected node category without prefix.
#' @param selected_subcats List. Selected sub-category values to include in graph. 
#' @param cat_prefix Character string. Category attribute prefix format to match. Default is \code{"vosonCA_"}.
#' 
#' @return An igraph graph object.
#' 
#' @examples
#' \dontrun{
#' # return a graph containing only nodes that have the node category 
#' # attribute "vosonCA_Stance" value "liberal"
#' g <- get_pkg_data("DividedTheyBlog_40Alist_release.graphml")
#' 
#' g <- filter_cats(g, "Stance", c("liberal"))
#' }
#' 
#' @export
filter_cats <- function(g, selected_cat, selected_subcats, cat_prefix = "vosonCA_") {
  
  if (selected_cat == "All") {
    return(g)
  }
  
  # re-create category node attribute name
  vattr <- paste0(cat_prefix, selected_cat)
  
  # remove All from sub-categories list
  selected_subcats <- selected_subcats[selected_subcats != "All"]
  
  # filter out all nodes that do not have a category value in sub-categories list
  if (length(selected_subcats) > 0) {
    g <- igraph::delete_vertices(g, igraph::V(g)[!(igraph::vertex_attr(g, vattr) %in% selected_subcats)])
  }
  
  g
}

#' @title Remove nodes from graph by node id
#'
#' @description This function removes a list of nodes from the graph object by node id value. 
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param ids List. Selected node ids to remove.
#' @param rm_iso Logical. Remove isolate nodes from graph (degree == 0).
#' 
#' @return An igraph graph object.
#' 
#' @export
filter_nodes <- function(g, ids = NULL, rm_iso = FALSE) {
  if (rm_iso) ids <- V(g)[degree(g) == 0]
  if (is.null(ids)) return(g)
  if (length(ids) > 0) {
    rm_ids <- which(igraph::V(g)$id %in% ids)
    g <- igraph::delete_vertices(g, rm_ids)
  }
  g
}

#' @title Load package included network graph
#'
#' @description This function loads a network graph included in the \code{extdata} directory of the 
#' \code{VOSONDash} package by file name. 
#' 
#' @param fname Character string. Name of demonstration \code{graphml} file.
#' 
#' @return An igraph graph object.
#' 
#' @examples
#' \dontrun{
#' # load the "Divided They Blog" package included network graph by file name
#' g <- get_pkg_data("DividedTheyBlog_40Alist_release.graphml")
#' }
#' 
#' @export
get_pkg_data <- function(fname) {
  tryCatch({
    f <- system.file("extdata", fname, package = "VOSONDash", mustWork = TRUE)
    g <- igraph::read_graph(f, format = c('graphml'))  
  }, error = function(e) {
    stop(e)
  })
  
  g
}

#' @title Load the package included "Divided They Blog" network graph
#'
#' @description This is a convenience function to load the "DividedTheyBlog_40Alist_release.graphml" graph. 
#' 
#' @return An igraph graph object.
#' 
#' @examples
#' \dontrun{
#' # load the "Divided They Blog" network graph
#' g <- get_dtb_graph()
#' }
#' 
#' @keywords internal
#' @export
get_dtb_graph <- function() {
  get_pkg_data("DividedTheyBlog_40Alist_release.graphml")
}

#' @title Load the package included "Enviro Activist Websites 2006" network graph
#'
#' @description This is a convenience function to load the "enviroActivistWebsites_2006.graphml" graph. 
#' 
#' @return An igraph graph object.
#' 
#' @examples
#' \dontrun{
#' # load the "Enviro Activist Websites 2006" network graph
#' g <- get_eaw_graph()
#' }
#' 
#' @keywords internal
#' @export
get_eaw_graph <- function() {
  get_pkg_data("enviroActivistWebsites_2006.graphml")
}
