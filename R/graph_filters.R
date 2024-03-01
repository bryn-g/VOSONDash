#' @title Get component count and min max size ranges for component modes
#'
#' @description This function gets the number of components and the minimum size and maximum size range for all
#'   components for a given mode.
#'
#' @param g \pkg{igraph} \code{graph} object.
#' @param mode Character string. Use strongly or weakly connected components by specifying \code{"strong"} or
#'   \code{"weak"}. Ignored for undirected graphs. Default is \code{"NULL"} for both modes.
#' @param graph Logical. Return graph with node community membership ids \code{"idc"}. Default is \code{"FALSE"}.

#' @return A named list of size and range values per component mode.
#'
#' @export
get_comps_range <- function(g, mode = NULL, graph = FALSE) {
  modes <- list(weak = "weak", strong = "strong")
  if (!is.null(mode)) modes <- modes[[which(names(modes) == mode)]]
  
  comp_ranges <- sapply(
    modes,
    function(x) {
      c <- igraph::components(g, mode = x)
      y <- list(
        no = c$no,
        min = suppressWarnings(min(c$csize)),
        max = suppressWarnings(max(c$csize)),
        csize = c$csize
      )
      if (graph) {
        y$g <- g
        igraph::V(y$g)$idc <- c$membership
      }
      y
    }, simplify = FALSE, USE.NAMES = TRUE)
  
  comp_ranges
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
  cc <- igraph::components(g, mode = mode)
  igraph::V(g)$idc <- cc$membership
  
  if (is.null(range) && is.null(ids)) return(g)
  
  rm_nodes <- c()
  
  if (!is.null(ids)) {
    rm_nodes <- V(g)[which(!cc$membership %in% ids)]$name
    
    if (is.null(range)) {
      g <- igraph::delete_vertices(g, which(igraph::V(g)$name %in% rm_nodes))
      return(g)
    }
  }
  
  min_range <- range[1]
  max_range <- range[2]
  
  min_cc_size <- suppressWarnings(min(cc$csize)) # suppress no non-missing arguments to min
  max_cc_size <- suppressWarnings(max(cc$csize)) # returning Inf warning
  
  nodes_lt_min_cc <- nodes_gt_max_cc <- NULL
  
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
  
  # named list - names are cluster, values are node names
  
  # remove nodes from graph
  if (length(rm_nodes) > 0) {
    rm_chk <- which(igraph::V(g)$name %in% rm_nodes)
    g <- igraph::delete_vertices(g, rm_chk)
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

#' @title Add centrality measures to graph as node attributes
#' 
#' @description Adds degree, in-degree, out-degree, betweenness and closeness measures to graph as node attributes.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#'
#' @return An igraph graph object.
#' 
#' @export
add_cent_measures <- function(g) {
  # add degree
  igraph::V(g)$degree <- igraph::degree(g, mode = "total")
  if (igraph::is_directed(g)) {
    igraph::V(g)$indegree <- igraph::degree(g, mode = "in")
    igraph::V(g)$outdegree <- igraph::degree(g, mode = "out")
  } else {
    igraph::V(g)$indegree <- igraph::V(g)$outdegree <- 0
  }
  
  # add centrality
  if (igraph::gorder(g) > 1) {
    igraph::V(g)$betweenness <- igraph::betweenness(g)
    igraph::V(g)$closeness <- suppressWarnings(igraph::closeness(g))
  } else {
    igraph::V(g)$betweenness <- igraph::V(g)$closeness <- 0
  }

  g
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
  iso_ids <- V(g)[which(igraph::degree(g) == 0)]$name
  
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
    rm_ids <- which(igraph::V(g)$name %in% ids)
    g <- igraph::delete_vertices(g, rm_ids)
  }
  g
}
