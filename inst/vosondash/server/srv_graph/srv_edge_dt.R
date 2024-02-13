# graph edges as dataframe
r_graph_edges_df <- reactive({
  g <- r_graph_filter()
  
  igraph::as_data_frame(g, what = c("edges"))
})

# graph edges data table
output$dt_edges <- DT::renderDataTable({
  data <- r_graph_edges_df()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_e_truncate_text_check == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    DT::datatable(data, extensions = 'Buttons', filter = "top", selection = "none", # rows not selectable
                  options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                                 columnDefs = col_defs, dom = 'lBfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'print')), class = 'cell-border stripe compact hover')
  }
})
