#' @title Get node attribute properties
#'
#' @description This function returns detected categorical, continuous, text and url type attributes.
#'
#' @param g \pkg{igraph} \code{graph} object.
#'
#' @return A tibble of detected node attribute properties.
#'
#' @export
get_node_properties <- function(g) {
  nodes_n <- igraph::gorder(g)
  attr_names <- igraph::vertex_attr_names(g)
  
  df_props <- tibble::tibble()
  
  for (i in seq_along(attr_names)) {
    attr_name <- attr_names[i]
    
    vals <- igraph::vertex_attr(g, attr_name)
    vals[vals == "NA"] <- NA
    
    if (is.logical(vals)) {
      df <- node_property("cat", attr_name, value = as.character(unique(vals)))
      
    } else if (is.character(vals)) {
      unique_vals <- unique(vals)
      
      # check num unique values per cat < 25% of num of nodes
      if ((length(unique_vals) > 1) & (length(unique_vals) <= (nodes_n/4))) {
        df <- node_property("cat", attr_name, value = unique_vals)
        
      } else if (all(is_url(unique_vals))) {
        type <- ifelse(all(is_img(unique_vals)), "img_url", "url")
        df <- node_property(type, attr_name)
        
      } else {
        df <- node_property("text", attr_name)
      }
  
    } else if (is.numeric(vals)) {
      df <- node_property("cont", attr_name)

    } else {
      df <- node_property("other", attr_name)
      
    }
    
    df_props <- df_props |> dplyr::bind_rows(df)
  }
  
  df_props
}

# get property tibble
node_property <- function(type, key, value = NA, color = NA) {
  tibble::tibble(type = type, key = key, value = value, color = color)
}

# check url values
is_url <- function(x) {
  x <- x[!is.na(x)]
  rx <- "^(https?)://"
  grepl(rx, x, ignore.case = TRUE, perl = TRUE)
}

# check for image types
is_img <- function(x) {
  x <- x[!is.na(x)]
  rx <- "\\.(gif|jpg|jpeg|png)$"
  grepl(rx, x, ignore.case = TRUE, perl = TRUE)
}
