# proxy for nodes data table used for row manipulation
dt_nodes_proxy <- dataTableProxy("dt_nodes")

# graph nodes as dataframe
r_graph_nodes_df <- reactive({
  g <- r_graph_filter()

  df_params <- list()
  
  df_params[["name"]] <- igraph::V(g)$name
  if (!(is.null(igraph::vertex_attr(g, "label")))) df_params[["label"]] <- igraph::V(g)$label
  if ("color" %in% igraph::vertex_attr_names(g)) df_params[["color"]] <- igraph::V(g)$color
  
  if (input$node_mtdn_img_chk) {
    if ("user.avatar" %in% igraph::vertex_attr_names(g)) {
      df_params[["user.avatar"]] <- igraph::V(g)$user.avatar
    }
  }
  
  df_params[["degree"]] <- igraph::V(g)$Degree
  df_params[["indegree"]] <- igraph::V(g)$Indegree
  df_params[["outdegree"]] <- igraph::V(g)$Outdegree
  df_params[["betweenness"]] <- igraph::V(g)$Betweenness
  df_params[["closeness"]] <- igraph::V(g)$Closeness
  
  attr_v <- igraph::vertex_attr_names(g)
  voson_txt_attrs <- attr_v[grep(voson_txt_prefix, attr_v, perl = T)]
  if (length(voson_txt_attrs)) {
    attr <- voson_txt_attrs[1]
    df_txt_attr <- gsub(voson_txt_prefix, "", attr, perl = TRUE)
    df_params[[df_txt_attr]] <- igraph::vertex_attr(g, attr, index = V(g))
  }
  
  voson_cat_attrs <- attr_v[grep(voson_cat_prefix, attr_v, perl = T)]
  if (length(voson_cat_attrs) > 0) {
    for (i in 1:length(voson_cat_attrs)) {
      attr <- voson_cat_attrs[i]
      df_txt_attr <- gsub(voson_cat_prefix, "", attr, perl = TRUE) # vosonCA_
      df_params[[df_txt_attr]] <- igraph::vertex_attr(g, attr, index = V(g))
    }  
  }
  
  for (attr in attr_v) {
    values <- igraph::vertex_attr(g, attr)
    if (is.numeric(values) &
        (!attr %in% voson_txt_attrs) &
        (!attr %in% voson_cat_attrs) &
        (!attr %in% names(df_params)) &
        (!tolower(attr) %in% names(df_params))) {
      df_params[[attr]] <- values
    }
  }
  
  df_params["stringsAsFactors"] <- FALSE
  df <- do.call(data.frame, df_params)
  
  row.names(df) <- igraph::V(g)$id
  
  df
})

# graph nodes data table
output$dt_nodes <- DT::renderDataTable({
  data <- r_graph_nodes_df()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_v_truncate_text_chk == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    dt <- DT::datatable(
      data,
      extensions = "Buttons",
      filter = "top",
      options = list(
        lengthMenu = gbl_dt_menu_len,
        pageLength = gbl_dt_page_len,
        scrollX = TRUE,
        columnDefs = col_defs,
        dom = "lBfrtip",
        buttons = c("copy", "csv", "excel", "print")
      ),
      class = "cell-border stripe compact hover"
    )
    
    # format betweeness and closeness values to display 3 decimal places
    if (nrow(data) > 0) {
      dt <- DT::formatRound(dt, columns = c("betweenness", "closeness"), digits = 3)
    }
    
    return(dt)
  }
  
  NULL
})
