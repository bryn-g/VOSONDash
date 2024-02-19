# graph edges as dataframe
r_graph_edges_df <- reactive({
  g <- r_graph_filter()
  
  edges <- igraph::as_data_frame(g, what = c("edges"))
  
  if ("id" %in% colnames(edges)) row.names(edges) <- igraph::E(g)$id
  
  edges
})

# graph edges data table
output$dt_edges <- DT::renderDataTable({
  data <- r_graph_edges_df()
  
  data <- data |> dplyr::relocate(id, label)
  
  # truncate text in column cells
  col_defs <- NULL
  
  if (input$graph_dt_e_truncate_text_chk == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  cols_hide <- match(c("id", "id.imp", "label.imp", "label"), colnames(data))
  col_defs <- append(col_defs, list(list(targets = cols_hide, visible = FALSE)))
  
  if (!is.null(data)) {
    DT::datatable(data, extensions = "Buttons", filter = "top", selection = "none", # rows not selectable
                  options = list(
                    lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                    columnDefs = col_defs,
                    dom = "lBfrtip",
                    buttons = list("copy", "csv", "excel", "print",
                      list(extend = "colvis", text = "Choose Columns", columns = c(1:length(colnames(data))))
                    )
                  ),
                  class = "cell-border stripe compact hover")
  }
})
