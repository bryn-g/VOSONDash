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
  }, error = function(err) {
    cat(file=stderr(), paste0("error reading package data: ", err))
    return(NULL)
  })
  
  g
}

#' @title Return a list of graphml files found in package extdata
#'
#' @description This function returns a list of file names for graphs included in the \code{extdata} directory of the
#'   \code{VOSONDash} package.
#'
#' @return A named list of file names.
#'
#' @export
get_pkg_data_list <- function() {
  f_list <- c()
  tryCatch({
    f_list <- list.files(
      path = system.file("extdata", "", package = "VOSONDash", mustWork = TRUE),
      pattern = "\\.graphml$"
    )
  }, error = function(err) {
    cat(file=stderr(), paste0("error finding package data sets: ", err))
  })
  
  if (length(f_list) < 1) return(NULL)
  
  f_names <- lapply(f_list, function(x) gsub("\\.graphml$", "", x, ignore.case = TRUE))
  names(f_list) <- f_names
  f_list
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
