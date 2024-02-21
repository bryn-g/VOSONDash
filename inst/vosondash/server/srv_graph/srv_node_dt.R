# proxy for nodes data table used for row manipulation
dt_nodes_proxy <- dataTableProxy("dt_nodes")

observeEvent(r_graph_filter(), {
  selected <- isolate(g_prune_rv$selected)
  if (!is.null(selected)) {
    g_prune_rv$restore <- selected
  } else {
    g_prune_rv$restore <- NULL
  }
}, ignoreInit = TRUE)

# graph nodes as dataframe
r_graph_nodes_df <- reactive({
  g <- req(r_graph_filter())

  nodes <- g |> igraph::as_data_frame(what = c("vertices"))
  nodes$idx <- 1:nrow(nodes)
  rownames(nodes) <- igraph::V(g)$id

  nodes <- nodes |> tibble::as_tibble(rownames = NA)
  
  nodes
})

# graph nodes data table
output$dt_nodes <- DT::renderDataTable({
  data <- r_graph_nodes_df()
  
  if (is.null(data)) return(NULL)
  
  data <- data |> dplyr::relocate(idx, label)
  
  # truncate text in column cells
  col_defs <- NULL
  if (input$graph_dt_v_truncate_text_chk == TRUE) {
    col_defs <- gbl_dt_col_defs
    col_defs[[1]]$targets <- "_all"
  }
  # browser()
  def_lst <- isolate(g_rv$attrs) |> dplyr::filter(unit == "node" & (type %in% c("cat", "cont"))) |> dplyr::pull("key") |> unique()
  def_lst <- c(c("idx", "idn"), c("degree", "indegree", "outdegree", "betweenness", "closeness"), def_lst)
  
  hide_lst <- seq_along(colnames(data))[-match(def_lst, colnames(data))]
  
  # cols_hide <- match(c("id", "id.imp", "label.imp", "label"), colnames(data))
  col_defs <- append(col_defs, list(list(targets = hide_lst, visible = FALSE)))
  
  pre_sel_nodes <- isolate(g_prune_rv$restore)
  sel_ids <- NULL
  if (!is.null(pre_sel_nodes)) {
    ids <- pre_sel_nodes |> dplyr::pull(id)
    sel_idx <- data |> dplyr::filter(id %in% ids) |> dplyr::pull(idx)
  }
  
  dt_selection <- list(target = "row")
  if (length(pre_sel_nodes)) dt_selection$selected <- as.numeric(sel_idx)
  
  dt <- DT::datatable(
    data,
    selection = dt_selection,
    extensions = c("Buttons", "ColReorder", "FixedColumns", "Responsive"),
    filter = "top",
    options = list(
      lengthMenu = gbl_dt_menu_len,
      pageLength = gbl_dt_page_len,
      scrollX = TRUE,
      fixedColumns = TRUE,
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
      ),
      colReorder = TRUE
    ),
    class = "cell-border stripe compact hover"
  )
  
  # format betweeness and closeness values to display 3 decimal places
  if (nrow(data) > 0) {
    if (all(c("betweenness", "closeness") %in% colnames(data))) {
      dt <- DT::formatRound(dt,
                            columns = c("betweenness", "closeness"),
                            digits = 3)
    }
  }
  
  dt

})
