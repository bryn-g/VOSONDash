init("init_graph", "reset_graph_ctrls")

# graph reactive variables
g_rv <- reactiveValues(
  data = NULL,
  seed = NULL,
  is_igraph = FALSE,
  layout = NULL,
  attrs = NULL,
  directed = NULL
)

# event for graph data set by upload or collection
observeEvent(g_rv$data, {
  req(g_rv$data, cancelOutput = TRUE)
  
  # check data is an igraph
  if (!VOSONDash::is_igraph(g_rv$data$data)) {
    cat(file=stderr(), paste0("g_rv$data: not igraph\n"))
    req(FALSE, cancelOutput = TRUE)
  }
  
  # set metadata
  g_meta_rv$data <- g_rv$data$meta
  
  # enable tabs
  removeCssClass(selector = "a[data-value = 'igraph']", class = "inactive_menu_link")
  removeCssClass(selector = "a[data-value = 'visNetwork']", class = "inactive_menu_link")
  removeCssClass(selector = "a[data-value = 'graph_info']", class = "inactive_menu_link")
  
  # set to network graph tab
  if (input$nav_sel_tab_id != "network_graphs_tab") {
    updateNavbarPage(session, "nav_sel_tab_id", selected = "network_graphs_tab")
  }
  
  # set to igraph plot tab
  if (!input$canvas_tab %in% c("igraph", "visNetwork")) {
    updateTabsetPanel(session, "canvas_tab", selected = "igraph")
  }
  
  trigger("reset_graph_ctrls")
  
}, ignoreInit = TRUE)

on("reset_graph_ctrls", {
  # reset graph rv
  g_prune_rv$nodes <- NULL
  g_prune_rv$prev_selected <- NULL
  
  g_filter_rv$active <- NULL
  g_comps_rv$pre_comps <- NULL
  g_comps_rv$fltr_range <- NULL
  
  # reset input controls
  reset_enable_g_ctrls()
  
  trigger("init_graph")
})

# set the base graph object from initial processing
r_graph_base <- reactive({
  watch("init_graph")

  g <- isolate(g_rv$data$data)
  if (!isTruthy(g)) return(NULL)
  
  # set up graph ids and labels
  g <- VOSONDash::set_ids_and_labels(g)
  
  # initial seed and layout
  seed <- sample(1:20000, 1)
  updateNumericInput(session, "graph_seed_input", value = seed)
  g_rv$seed <- seed
  layout <- ifelse(isTruthy(isolate(input$graph_layout_select)), isolate(input$graph_layout_select), "FR")
  layout <- f_get_layout(g, seed, layout)
  g_layout_rv$coords_base <- f_get_coords(g, layout)
  
  # get graph attributes
  g_rv$attrs <- VOSONDash::get_attr_properties(g)
  g_rv$directed <- igraph::is_directed(g)
  
  g
})

# get attributes by unit
f_get_attrs <- function(df, unit = "node") {
  df |> dplyr::filter(unit == {{ unit }}) |> dplyr::pull("key") |> unique()
}

# graph attributes changed
observeEvent(g_rv$attrs, {
  node_attrs <- f_get_attrs(g_rv$attrs, "node")
  
  g_nodes_rv$attrs <- node_attrs
  g_nodes_rv$cats <- f_get_cats(g_rv$attrs)
  g_nodes_rv$labels <- sort(node_attrs[!node_attrs == "label"])
  
  if ("color.imp" %in% node_attrs) set_ctrl_state(node_g_color_ctrls(), "enable")
  
  edge_attrs <- f_get_attrs(g_rv$attrs, "edge")
  g_edges_rv$attrs <- edge_attrs
  g_edges_rv$labels <- sort(edge_attrs[!edge_attrs == "label"])
  
}, ignoreInit = TRUE)

# set node coords to base
observeEvent(g_layout_rv$coords_base, {
  g_layout_rv$coords <- g_layout_rv$coords_base
}, ignoreInit = TRUE, ignoreNULL = TRUE)
