# proxy for nodes data table used for row manipulation
dt_nodes_proxy <- dataTableProxy("dt_nodes")

# graph nodes as dataframe
r_graph_nodes_df <- reactive({
  g <- r_graph_filter()

  nodes <- g |> igraph::as_data_frame(what = c("vertices"))
  
  if ("id.n" %in% colnames(nodes)) {
    row.names(nodes) <- igraph::V(g)$id.n
  } else {
    row.names(nodes) <- igraph::V(g)$id
  }

  nodes
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
  # list(targets = {{some vector of 0-based column numbers}}, visible = FALSE)
  
  # buttons = list(list(extend = 'colvis', text='Column Picker', columns = c(1:19, 23:24)))
  
  if (!is.null(data)) {
    data <- data |> dplyr::relocate(id, label)
    
    cols_hide <- match(c("name", "id.n", "id.imp", "label.imp", "label"), colnames(data))
    col_defs <- append(col_defs, list(list(targets = cols_hide, visible = FALSE)))
    
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
        buttons = list(
          "copy",
          "csv",
          "excel",
          "print",
          list(
            extend = "colvis",
            text = "Choose Columns",
            columns = c(1:length(colnames(data)))
          )
        )
      ),
      class = "cell-border stripe compact hover"
    )
    
    # format betweeness and closeness values to display 3 decimal places
    if (nrow(data) > 0) {
      if (all(c("betweenness", "closeness") %in% colnames(data))) {
        dt <- DT::formatRound(dt, columns = c("betweenness", "closeness"), digits = 3)
      }
    }
    
    return(dt)
  }
  
  NULL
})
