#' @title Get component count and min max size ranges for component modes
#'
#' @description This function gets the number of components and the minimum size and maximum size range for all
#'   components for a given mode.
#'
#' @param g \pkg{igraph} \code{graph} object.
#' @param mode Character string. Use strongly or weakly connected components by specifying \code{"strong"} or
#'   \code{"weak"}. Ignored for undirected graphs. Default is \code{"NULL"} for both modes.
#'
#' @return A named list of size and range values per component mode.
#'
#' @export
get_comps_range <- function(g, mode = NULL) {
  modes <- list(weak = "weak", strong = "strong")
  if (!is.null(mode)) modes <- modes[[which(names(modes) == mode)]]
  
  comp_ranges <- sapply(
    modes,
    function(x) {
      c <- igraph::components(g, mode = x)
      list(
        no = c$no,
        min = suppressWarnings(min(c$csize)),
        max = suppressWarnings(max(c$csize)),
        csize = c$csize
      )
    }, simplify = FALSE, USE.NAMES = TRUE)  
}

#' @title Filter out graph nodes not in component size range
#' 
#' @description This function removes any graph nodes that are in components that fall outside of the specified 
#' component size range.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param mode Character string. Use strongly or weakly connected components by specifying \code{"strong"} or 
#' \code{"weak"}. Ignored for undirected graphs. Default is \code{"strong"}.
#' @param range Numeric vector. Min and max values or size range of component.
#' @param ids Numeric vector. Retun only nodes belonging to component ids.
#'
#' @return An igraph graph object.
#' 
#' @export
filter_comps <- function(g, mode = "strong", range = NULL, ids = NULL) {
  if (is.null(range) && is.null(ids)) return(NULL)
    
  min_range <- range[1]
  max_range <- range[2]
  
  cc <- igraph::components(g, mode = mode)
  igraph::V(g)$c_id <- cc$membership
  
  min_cc_size <- suppressWarnings(min(cc$csize)) # suppress no non-missing arguments to min
  max_cc_size <- suppressWarnings(max(cc$csize)) # returning Inf warning
  
  nodes_lt_min_cc <- nodes_gt_max_cc <- NULL
  rm_nodes <- c()
  
  # remove nodes not part of components in component size range by name
  
  # add nodes in components < range min to list
  if (min_range > min_cc_size) {
    nodes_lt_min_cc <- names(which(table(cc$membership) < min_range))
    
    if (length(nodes_lt_min_cc) > 0) {
      rm_nodes <- sapply(nodes_lt_min_cc, function(x) append(rm_nodes, names(which(cc$membership == x))))
    }
  }
  
  # add nodes in components > range max to list
  if (max_range < max_cc_size) {
    nodes_gt_max_cc <- names(which(table(cc$membership) > max_range))
    
    if (length(nodes_gt_max_cc) > 0) {
      rm_nodes <- sapply(nodes_gt_max_cc, function(x) append(rm_nodes, names(which(cc$membership == x))))
    }
  }
  
  if (!is.null(ids)) {
    rm_nodes <- unique(append(rm_nodes, which(!cc$membership %in% ids)))
  }
  
  # remove nodes from graph
  if (length(rm_nodes) > 0) g <- igraph::delete_vertices(g, unlist(rm_nodes))
  
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
get_node_cats <- function(g, cat_prefix = "vosonCA_", cat_colors = FALSE) {
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
  
  attr_e <- igraph::edge_attr_names(g)
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
  if (selected_cat == "All") return(g)
  
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

#' @title Remove isolate nodes from graph
#'
#' @description This function removes isolate nodes from the graph object. 
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param loops Logical. Whether to include loop edges in node degree calculation.
#' 
#' @return An igraph graph object.
#' 
#' @export
filter_nodes_iso <- function(g, loops = FALSE) {
  iso_ids <- V(g)[which(igraph::degree(g) == 0)]$id
  
  if (!length(iso_ids)) return(g)
  
  filter_nodes(g, ids = iso_ids)
}

#' @title Remove nodes from graph by node id
#'
#' @description This function removes a list of nodes from the graph object by node id value. 
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param ids List. Selected node ids to remove.
#' 
#' @return An igraph graph object.
#' 
#' @export
filter_nodes <- function(g, ids = NULL) {
  if (is.null(ids)) return(g)
  
  if (length(ids) > 0) {
    rm_ids <- which(igraph::V(g)$id %in% ids)
    g <- igraph::delete_vertices(g, rm_ids)
  }
  g
}
