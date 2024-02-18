#' @title Get properties of node and edge attributes
#'
#' @description This function returns detected categorical, continuous, text and url type attributes.
#'
#' @param g \pkg{igraph} \code{graph} object.
#' @param type Character vector. Can contain "node" or "edge" as types. Default is \code{NULL} for both.
#' @param cat_max Numeric. Maximum number of unique values an attribute can have to be considered a category. By default
#'   a category must have >= 1 and <= 25% of its total values unique. Default is \code{NULL}.
#'
#' @return A tibble of attribute properties.
#'
#' @export
get_attr_properties <- function(g, type = NULL, cat_max = NULL) {
  if (is.null(type)) {
    type <- c("node", "edge")
  }
  
  df_props <- tibble::tibble()
  
  for (i in seq_along(type)) {
    unit <- type[i]

    f_attr_names <- ifelse(unit == "node", igraph::vertex_attr_names, igraph::edge_attr_names)
    f_attr_vals <- ifelse(unit == "node", igraph::vertex_attr, igraph::edge_attr)
    unit_n <- ifelse(unit == "node", igraph::gorder(g), igraph::gsize(g))
    
    attr_names <- f_attr_names(g)
    
    for (j in seq_along(attr_names)) {
      attr_name <- attr_names[j]
      
      vals <- f_attr_vals(g, attr_name)
      vals[vals == "NA"] <- NA
      
      if (is.logical(vals)) {
        df <- attr_property(unit, "cat", attr_name, value = as.character(unique(vals)))

      } else if (is.character(vals)) {
        unique_vals <- unique(vals)
        
        cat_max <- round(unit_n/4)
        if (!is.null(cat_max) && is.numeric(cat_max)) {
          cat_max <- round(cat_max)
        }
        
        if ((length(unique_vals) >= 1) & (length(unique_vals) <= cat_max)) {
          df <- attr_property(unit, "cat", attr_name, value = unique_vals)
            
        } else if (all(is_url(unique_vals))) {
          url_type <- ifelse(all(is_img(unique_vals)), "img_url", "url")
          df <- attr_property(unit, url_type, attr_name)
          
        } else {
          df <- attr_property(unit, "text", attr_name)
        }
        
      } else if (is.numeric(vals)) {
        df <- attr_property(unit, "cont", attr_name)
        
      } else {
        df <- attr_property(unit, "other", attr_name)
        
      }
      
      df_props <- df_props |> dplyr::bind_rows(df)
    }
  }
  
  df_props
}

# get property tibble
attr_property <- function(unit, type, key, value = NA) {
  tibble::tibble(unit = unit, type = type, key = key, value = value)
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
