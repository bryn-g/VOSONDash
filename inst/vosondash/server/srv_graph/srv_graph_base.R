# graph reactive variables
g_rv <- reactiveValues(
  data = NULL,
  dir = NULL,
  seed = NULL,
  is_igraph = FALSE,
  layout = NULL
)

# check input igraph data
r_is_data_valid <- reactive({
  data <- req(g_rv$data$data)
  ifelse((isTruthy(data) & ("igraph" %in% class(data))), TRUE, FALSE)
})

# event for graph data set by upload or collection
observeEvent(g_rv$data, {
  if (!r_is_data_valid()) {
    dash_logger("attempted to set invalid igraph data.")
    return()
  }
  
  removeCssClass(selector = "a[data-value = 'igraph']", class = "inactive_menu_link")
  removeCssClass(selector = "a[data-value = 'visNetwork']", class = "inactive_menu_link")
  removeCssClass(selector = "a[data-value = 'graph_info']", class = "inactive_menu_link")
  
  g_meta_rv$data <- g_rv$data$meta
  dash_logger(paste0("igraph data object loaded. ", g_meta_rv$data$name))
  
  g_nodes_rv$pruned <- c()
  dt_prev_sel$nodes <- c()
  
  reset_enable_g_ctrls()
  
  if (input$nav_sel_tab_id != "network_graphs_tab") {
    updateNavbarPage(session, "nav_sel_tab_id", selected = "network_graphs_tab")
  }
  
  if (!input$canvas_tab %in% c("igraph", "visNetwork")) {
    updateTabsetPanel(session, "canvas_tab", selected = "igraph")
  }
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# set the base graph object from initial processing
r_graph_base <- reactive({
  req(r_is_data_valid())
  
  # g <- g_rv$data$data
  g <- tidygraph::as_tbl_graph(g_rv$data$data)
  g <- f_set_id_and_label(g)
  
  seed <- sample(1:20000, 1)
  updateNumericInput(session, "graph_seed_input", value = seed)
  g_rv$seed <- seed
  layout <- ifelse(isTruthy(isolate(input$graph_layout_select)), isolate(input$graph_layout_select), "FR")
  layout <- f_get_layout(g, seed, layout)
  g_layout_rv$coords_base <- f_get_coords(g, layout)
  
  # g_nodes_rv$cats <- VOSONDash::get_node_cats(g)
  g_nodes_rv$properties <- VOSONDash::get_attr_properties(g)
  
  g
})

observeEvent(g_layout_rv$coords_base, {
  g_layout_rv$coords <- g_layout_rv$coords_base
})
