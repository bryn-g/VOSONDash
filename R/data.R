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
