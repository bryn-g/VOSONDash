# graph reactive variables
g_prune_rv <- reactiveValues(
  nodes = NULL,
  selected = NULL,
  prev_selected = NULL,
  restore = NULL,
  action = NULL
)

r_rm_nodes_dt <- reactive({
  pruned <- g_prune_rv$nodes
  selected <- g_prune_rv$selected
  
  df <- NULL
  if (!is.null(pruned) && nrow(pruned)) df <- pruned
  if (!is.null(selected) && nrow(selected)) df <- df |> dplyr::bind_rows(selected)

  df
})

# # add selected data table rows to pruned nodes list
observeEvent(input$rm_sel_rows_btn, {
  g_prune_rv$action <- r_rm_nodes()
}, ignoreInit = TRUE)

# remove selected nodes
r_rm_nodes <- reactive({
  pruned <- g_prune_rv$nodes
  selected <- g_prune_rv$selected
  
  # clear seleect list
  if (!is.null(selected)) {
    selected <- selected |> dplyr::mutate(status = "pending")
    pruned <- selected |> dplyr::bind_rows(pruned)
    
    DT::selectRows(dt_nodes_proxy, NULL)
  }
  
  if (!is.null(pruned)) {
    # rm_nodes <- pruned |> dplyr::filter(status != "removed")
    
    # nodes pending removal
    # if (nrow(rm_nodes)) {
      g_prune_rv$nodes <- pruned
      g_prune_rv$selected <- NULL
      
      # return(rm_nodes)
      return(pruned)
    # }
  }
  
  NULL
})

# when selection changes
observeEvent(input$dt_nodes_rows_selected, {
  selected <- input$dt_nodes_rows_selected
  
  nodes <- r_graph_nodes_df()
  prune_lst <- g_prune_rv$nodes
  
  df <- NULL
  if (!is.null(selected) && length(selected)) {
    df <- nodes |> 
      tibble::as_tibble() |>
      dplyr::slice(selected) |>
      dplyr::select("idx", "id", "name") |>
      dplyr::mutate(status = "selected")
    
    # remove from seleect list if already in prune list 
    if (!is.null(prune_lst)) {
      df <- df |> dplyr::filter(!id %in% (prune_lst |> dplyr::pull("id")))
      DT::selectRows(dt_nodes_proxy, df |> dplyr::pull("idx"))
    }
  }
  
  g_prune_rv$selected <-df
}, ignoreInit = TRUE, ignoreNULL = FALSE)

# graph nodes data table
output$dt_nodes_rm <- DT::renderDataTable({
  data <- r_rm_nodes_dt()

  if (!is.null(data)) {
    data <- data |> tibble::column_to_rownames(var = "id")
    selection <- list(target = "row", c())
  } else {
    data <- tibble::tibble(data = "no nodes selected.")
    selection <- "none"
  }
  
  dt <- DT::datatable(
    data,
    extensions = c("FixedColumns", "Responsive"),
    selection = selection,
    options = list(
      lengthMenu = list(c(5, 10, 15, 20, 25, 50, 100, -1), c("5", "10", "15", "20", "25", "50", "100", "All")),
      pageLength = 5,
      scrollX = TRUE,
      dom = "lftip"
    ),
    class = "cell-border stripe compact hover"
  )
  
  dt
})
