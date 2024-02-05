# proxy for nodes data table used for row manipulation
dt_nodes_proxy <- dataTableProxy("dt_nodes")

# graph nodes data table
output$dt_nodes <- DT::renderDataTable({
  data <- r_graph_nodes_df()
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_v_truncate_text_check == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  
  if (!is.null(data)) {
    dt <- DT::datatable(data, extensions = 'Buttons', filter = "top",
                        options = list(lengthMenu = gbl_dt_menu_len, pageLength = gbl_dt_page_len, scrollX = TRUE,
                                       columnDefs = col_defs, dom = 'lBfrtip', buttons = c('copy', 'csv', 'excel', 'print')),
                        class = 'cell-border stripe compact hover')
    
    # format betweeness and closeness values to display 3 decimal places
    if (nrow(data) > 0) {
      dt <- DT::formatRound(dt, columns = c('betweenness', 'closeness'), digits = 3)
    }
    
    return(dt)
  }
  
  NULL
})