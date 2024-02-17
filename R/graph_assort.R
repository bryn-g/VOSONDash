#' @title Create a mixing matrix
#' 
#' @description Function creates a mixing matrix by graph node attribute. 
#' 
#' @note Mixing matrix original function written by Gary Weissman. See: https://gist.github.com/gweissman/2402741.
#'
#' @param g \pkg{igraph} graph object.
#' @param node_attr Character string. Node attribute or category.
#' @param use_density Logical. Use edge density. Default is \code{TRUE}.
#' 
#' @return A mixing matrix.
#'
#' @examples
#' \dontrun{
#' # create a mixing matrix of the demonstration network based on node 
#' # categorical attribute for political stance "vosonCA_Stance"
#' g <- get_pkg_data("DividedTheyBlog_40Alist_release.graphml")
#' 
#' mm <- mixmat(g, "vosonCA_Stance", use_density = FALSE)
#' }
#' 
#' @export
mixmat <- function(g, node_attr, use_density = TRUE) {
  # get unique list of characteristics of the attribute
  attr_lst <- sort(unique(igraph::vertex_attr(g, node_attr)))
  
  attr_n <- length(attr_lst)
  
  # mixing matrix by attribute
  mm <- matrix(nrow = attr_n, 
               ncol = attr_n, 
               dimnames = list(attr_lst, attr_lst))
  
  el <- igraph::as_edgelist(g, names = FALSE)
  for (i in 1:attr_n) {
    for (j in 1:attr_n) {
      mm[i, j] <- length(which(apply(el, 1, function(x) {
        igraph::vertex_attr(g, node_attr, x[1] ) == attr_lst[i] &&
          igraph::vertex_attr(g, node_attr, x[2] ) == attr_lst[j] } )))
    }
  }
  
  # use density or raw number of edges
  if (use_density) {
    return(mm / igraph::gsize(g))
  }
  
  mm
}
