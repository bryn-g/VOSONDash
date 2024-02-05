graph_rv <- reactiveValues(
  data = NULL,
  graph_dir = NULL,
  
  plot_height = gbl_plot_height,
  legend_height = 42,
  graph_seed = NULL,
  
  graph_cats = c(),
  graph_cat_selected = "",
  
  prune_nodes = c()
)

observeEvent(graph_rv$data, {
  req(graph_rv$data)
  updateNavbarPage(session, "nav_sel_tab_id", selected = "network_graphs_tab")
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# set the base graph and initiate ui
r_graph_base <- reactive({
  req(graph_rv$data)
  
  # clean base graph
  g <- graph_rv$data$data
  
  if (!"igraph" %in% class(g)) return(NULL)
    
  # add node ids and labels if not present
  attr_v <- igraph::vertex_attr_names(g)
  if (!("id" %in% attr_v)) {
    igraph::V(g)$id <- paste0("n", as.numeric(igraph::V(g))-1) # n0, n1 ..
  }
  
  if ("label" %in% attr_v) {
    igraph::V(g)$imported_label <- igraph::V(g)$label
  } else {
    # if no labels set label to node name
    igraph::V(g)$label <- ifelse(nchar(igraph::V(g)$name) > 0, igraph::V(g)$name, "-")
  }
  
  # graph directed
  graph_rv$graph_dir <- igraph::is_directed(g)
  graph_rv$graph_seed <- sample(1:20000, 1)
  
  # graph components
  f_set_comp_ranges(g)
  comp_rv$mode <- "weak"
  f_set_comp_slider_range()
  
  f_reset_filter_ctrls(g)
  
  graph_rv$graph_cats <- getNodeCategories(g)
  graph_rv$graph_cat_selected <- ""
  
  #setGraphTabControls()
  # get ui seed and layout and keep full node positions
  cat(file=stderr(), "running r_graph_base\n")
  
  g  
})

# init disable tabs with css
addCssClass(selector = "a[data-value = 'metrics_tab_panel']", class = "inactive_menu_link")
addCssClass(selector = "a[data-value = 'assort_tab_panel']", class = "inactive_menu_link")

# generate a new random seed
observeEvent(input$graph_reseed_button, graph_rv$graph_seed <- sample(1:20000, 1))

# update seed input
observeEvent(graph_rv$graph_seed, updateNumericInput(session, "graph_seed_input", value = graph_rv$graph_seed))

# set seed value
observeEvent(input$graph_seed_set_button, {
  req(input$graph_seed_input)
  if (is.numeric(input$graph_seed_input) && (input$graph_seed_input > 0) && !is.infinite(input$graph_seed_input)) {
    graph_rv$graph_seed <- round(input$graph_seed_input, digits = 0)
  }
})

# update plot height
observeEvent(input$plot_height, {
  graph_rv$plot_height <- input$plot_height
}, ignoreInit = TRUE)

# update legend position
observeEvent(input$visnet_id_select_check, {
  graph_rv$legend_height <- ifelse(input$visnet_id_select_check, 85, 42)
})

# set hide state for overlays
observeEvent(c(
  input$selected_graph_tab,
  input$overlay_summary_chk,
  input$overlay_dl_btns_chk,
  input$overlay_legend_chk
  ), {
    req(input$selected_graph_tab)
    tab <- input$selected_graph_tab
    
    shinyjs::toggle("graph_summary_ui", condition = input$overlay_summary_chk)
    shinyjs::toggle("graph_legend_ui", condition = input$overlay_legend_chk)
    shinyjs::toggle("graph_graphml_dl_btn", condition = input$overlay_dl_btns_chk)
    
    if (!(tab %in% c("igraph", "visNetwork"))) {
      shinyjs::hide("graph_summary_ui")
      shinyjs::hide("graph_legend_ui")
      shinyjs::hide("graph_graphml_dl_btn")
      shinyjs::hide("visnet_html_dl_btn")
    }
      
    if (tab == "igraph") {
      if (input$overlay_legend_chk) graph_rv$legend_height <- 42
      shinyjs::disable("visnet_html_dl_btn")
      shinyjs::hide("visnet_html_dl_btn")
    } else if (tab == "visNetwork") {
      if (input$overlay_legend_chk) graph_rv$legend_height <- ifelse(input$visnet_id_select_check, 85, 42)
      shinyjs::enable("visnet_html_dl_btn")
      shinyjs::toggle("visnet_html_dl_btn", condition = input$overlay_dl_btns_chk)
    }
})

# graph summary text
r_graph_summary_html <- reactive({
  g <- req(r_graph_filtered())
  paste0(c(
    paste("Nodes:", igraph::vcount(g)),
    paste("Edges:", igraph::ecount(g)),
    paste("Isolates:", sum(igraph::degree(g) == 0))
  ), collapse = "<br>")
})


# graph edges as dataframe
r_graph_edges_df <- reactive({
  g <- req(r_graph_filtered())
  igraph::as_data_frame(g, what = c("edges"))
})

# graph nodes as dataframe
r_graph_nodes_df <- reactive({
  g <- req(r_graph_filtered())
  
  df_parameters <- list()
  
  df_parameters[["name"]] <- igraph::V(g)$name
  if (!(is.null(igraph::vertex_attr(g, "label")))) { df_parameters[["label"]] <- igraph::V(g)$label }
  if ("color" %in% igraph::vertex_attr_names(g)) { df_parameters[["color"]] <- igraph::V(g)$color }
  
  if (input$mast_images) {
    if ("user.avatar" %in% igraph::vertex_attr_names(g)) {
      df_parameters[["user.avatar"]] <- igraph::V(g)$user.avatar
    }
  }
  
  df_parameters[["degree"]] <- igraph::V(g)$Degree
  df_parameters[["indegree"]] <- igraph::V(g)$Indegree
  df_parameters[["outdegree"]] <- igraph::V(g)$Outdegree
  df_parameters[["betweenness"]] <- igraph::V(g)$Betweenness
  df_parameters[["closeness"]] <- igraph::V(g)$Closeness
  
  attr_v <- igraph::vertex_attr_names(g)
  voson_txt_attrs <- attr_v[grep(voson_txt_prefix, attr_v, perl = T)]
  if (length(voson_txt_attrs)) {
    attr <- voson_txt_attrs[1]
    df_txt_attr <- gsub(voson_txt_prefix, "", attr, perl = TRUE)
    df_parameters[[df_txt_attr]] <- igraph::vertex_attr(g, attr, index = V(g))
  }
  
  voson_cat_attrs <- attr_v[grep(voson_cat_prefix, attr_v, perl = T)]
  if (length(voson_cat_attrs) > 0) {
    for (i in 1:length(voson_cat_attrs)) {
      attr <- voson_cat_attrs[i]
      df_txt_attr <- gsub(voson_cat_prefix, "", attr, perl = TRUE) # vosonCA_
      df_parameters[[df_txt_attr]] <- igraph::vertex_attr(g, attr, index = V(g))
    }  
  }
  
  for (attr in attr_v) {
    values <- igraph::vertex_attr(g, attr)
    if (is.numeric(values) &
        (!attr %in% voson_txt_attrs) &
        (!attr %in% voson_cat_attrs) &
        (!attr %in% names(df_parameters)) &
        (!tolower(attr) %in% names(df_parameters))) {
      df_parameters[[attr]] <- values
    }
  }
  
  df_parameters["stringsAsFactors"] <- FALSE
  df <- do.call(data.frame, df_parameters)
  
  row.names(df) <- igraph::V(g)$id
  
  return(df)
})



# observeEvent(r_graph_base , {
#   r_node_cat_lst()
#   f_reset_filter_ctrls()
# })

# base graph node category list
r_node_cat_lst <- reactive({
  g <- req(r_graph_base())
  
  getNodeCategories(g)
})

# base graph node attribute list
r_node_attr_lst <- reactive({
  g <- req(r_graph_base())
  
  node_attr <- igraph::vertex_attr_names(g)
  init_attr_sel <- ifelse("imported_label" %in% node_attr, "imported_label", "id")
  node_attr <- sort(node_attr[!node_attr %in% c("label")])
  
  list(attrs = node_attr, sel = init_attr_sel)
})

# update node attribute label select
observeEvent(r_node_attr_lst(), {
  shinyjs::enable("node_label_sel")
  updateSelectInput(
    session,
    "node_label_sel",
    label = NULL,
    choices = c("None", r_node_attr_lst()$attrs),
    selected = r_node_attr_lst()$sel
  )
})

# r_graph_labels <- reactive({
#   g <- req(graph_rv$data)
#   
#   # add node ids and labels if not present
#   attr_v <- igraph::vertex_attr_names(g)
#   if (!("id" %in% attr_v)) {
#     igraph::V(g)$id <- paste0("n", as.numeric(igraph::V(g))-1) # n0, n1 ..
#   }
#   
#   if ("label" %in% attr_v) {
#     igraph::V(g)$imported_label <- igraph::V(g)$label
#   } else {
#     # if no labels set label to node name
#     igraph::V(g)$label <- ifelse(nchar(igraph::V(g)$name) > 0, igraph::V(g)$name, "-")
#   }
#   
#   g
# }) # > bindEvent(graph_rv$data)

# # when graphml data loaded or changed
# observeEvent(r_graph_labels(), { #graph_rv$data, {
#   g <- req(r_graph_labels())
#   #g <- req(graph_rv$data)
#     
#   # add node ids and labels if not present
#   # attr_v <- igraph::vertex_attr_names(g)
#   # if (!("id" %in% attr_v)) {
#   #   igraph::V(g)$id <- paste0("n", as.numeric(igraph::V(g))-1) # n0, n1 ..
#   # }
#   # 
#   # if ("label" %in% attr_v) {
#   #   # replace empty string labels
#   #   igraph::V(g)$label <- ifelse(nchar(igraph::V(g)$label) > 0, igraph::V(g)$label, "-")
#   # } else {
#   #   # if no labels set label to node name
#   #   igraph::V(g)$label <- ifelse(nchar(igraph::V(g)$name) > 0, igraph::V(g)$name, "-")
#   # }
#   
#   # set directed
#   #graph_rv$graph_dir <- igraph::is_directed(g)
#   
#   # enable network metrics tab
#   #removeCssClass(selector = "a[data-value = 'metrics_tab_panel']", class = "inactive_menu_link")
#   
#   # set directed
#   graph_rv$graph_dir <- igraph::is_directed(g)
#   
#   # enable network metrics tab
#   #removeCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
#   
#   setComponentRanges(g, "weak")
#   f_set_comp_slider_range()
# })



# # apply all filters to graph data and return modified graph
# graphFilters <- reactive({
#   g <- NULL
#   
#   if (!is.null(graph_rv$data)) {
#     g <- graph_rv$data
#     
#     # ----
#     # add node ids and labels if not present
#     attr_v <- igraph::vertex_attr_names(g)
#     if (!("id" %in% attr_v)) {
#       igraph::V(g)$id <- paste0("n", as.numeric(igraph::V(g))-1) # n0, n1 ..
#     }
#     
#     if ("label" %in% attr_v) {
#       # replace empty string labels
#       igraph::V(g)$label <- ifelse(nchar(igraph::V(g)$label) > 0, igraph::V(g)$label, "-")
#     } else {
#       # if no labels set label to node name
#       igraph::V(g)$label <- ifelse(nchar(igraph::V(g)$name) > 0, igraph::V(g)$name, "-")
#     }    
#     # ----
#     
#     g <- applyPruneFilterSrv(g, graph_rv$prune_nodes)
#     g <- applyCategoricalFilters(g, input$graph_cat_select, input$graph_sub_cats_select)
#     # isolate as graph_comp_type_sel has event
#     g <- applyComponentFilter(g, isolate(input$graph_comp_type_sel), input$graph_comp_slider)    
#     g <- applyGraphFilters(g, input$graph_isolates_check, input$graph_multi_edge_check, 
#                            input$graph_loops_edge_check)
#     g <- addAdditionalMeasures(g)
#     
#     # enable network metrics tab
#     removeCssClass(selector = "a[data-value = 'metrics_tab_panel']", class = "inactive_menu_link")
#   }
#   
#   return(g)
# })

## GRAPH COMPONENT CHANGE -----------------------------------

# ** check this is not redundant
# update component slider when graph component or category changed
# observeEvent({ input$graph_comp_type_sel
#   input$graph_sub_cats_select
#   graph_rv$prune_nodes
#   input$reset_on_change_check }, {
#     
#     g <- graph_rv$data
#     g <- applyPruneFilterSrv(g, graph_rv$prune_nodes)
#     
#     if (input$reset_on_change_check == TRUE) {
#       g <- applyCategoricalFilters(g, input$graph_cat_select, input$graph_sub_cats_select)
#     }
#     
#     f_update_comp_slider_range(g, input$graph_comp_type_sel)
#   }, ignoreInit = TRUE)

# r_comp_summary_txt <- reactive({
#   g <- graphFilters()
#   
#   output <- c()
#   
#   if (!is.null(g)) {
#     graph_clusters <- components(g, mode = isolate(input$graph_comp_type_sel))
#     
#     output <- append(output, paste0("Components (", isolate(input$graph_comp_type_sel), "): ", 
#                                     graph_clusters$no))
#     
#     min_value <- max_value <- 0
#     if (graph_clusters$no > 0) {
#       # suppress no non-missing arguments to min; returning Inf warning
#       min_value <- suppressWarnings(min(graph_clusters$csize))
#       max_value <- suppressWarnings(max(graph_clusters$csize))
#     }
#     
#     if (graph_clusters$no == 1) {
#       output <- append(output, paste0("Size: ", min_value, sep = ""))
#     } else {
#       output <- append(output, paste0("Size min: ", min_value, " max: ", max_value, sep = ""))
#     }
#   }else {
#     output <- append(output, paste0(""))
#   }
#   
#   paste0(output, collapse = '\n')
# })



## GRAPH CATEGORY CHANGE -------------------------------------


## GRAPH NODE PROPERTIES ------------------------------------------------

observeEvent(input$node_index_check, {
  if (input$node_index_check) {
    updateCheckboxInput(session, "node_labels_check", value = FALSE)
  }
})

observeEvent(input$node_labels_check, {
  if (input$node_labels_check) {
    updateCheckboxInput(session, "node_index_check", value = FALSE)
  }  
})

observeEvent(input$node_label_sel, {
  g <- req(r_graph_base())
  igraph::V(g)$label <- igraph::vertex_attr(g, input$node_label_sel)
}, ignoreInit = TRUE)

# observeEvent(input$node_label_sel, {
#   if (!is.null(graph_rv$data)) {
#     igraph::V(graph_rv$data)$label <- igraph::vertex_attr(graph_rv$data, input$node_label_sel)
#   }
# }, ignoreInit = TRUE)

# reset node size slider when changed to none
observeEvent(input$graph_node_size_select, {
  if (input$graph_node_size_select == "None") { 
    shinyjs::reset("graph_node_size_slider")
  }
})

# normalize continuous values
norm_values <- function(x) {
  # all values the same
  if (var(x) == 0) return(rep(0.1, length(x)))
  
  min_x <- min(x)
  diff_x <- max(x) - min_x
  s <- sapply(x, function(y) ((y - min_x) / diff_x))
}

# observeEvent(node_label_sel_lst(), {
#   freezeReactiveValue(input, "node_label_sel")
#   labels <- node_label_sel_lst()
#   sel <- ifelse("imported_label" %in% labels, "imported_label", "id")
#   updateSelectInput(session, "node_label_sel", label = NULL, choices = labels, selected = sel)
# }, ignoreInit = TRUE)
# 
# preserve_labels <- function(g) {
#   if ("label" %in% igraph::vertex_attr_names(g)) igraph::V(g)$imported_label <- igraph::V(g)$label
#   g
# }

# cont_attr_sel_lst <- reactive({
#   g <- graph_rv$data
#   if (is.null(g)) return(NULL)
#   attr_names <- sapply(
#     igraph::vertex_attr_names(g),
#     function(x) {
#       if (all(sapply(igraph::vertex_attr(g, x), is.numeric))) x
#     }
#   )
# })
# 
# observeEvent(cont_attr_sel_lst(), {
#   freezeReactiveValue(input, "graph_node_size_select")
#   updateSelectInput(session,
#                     "graph_node_size_select",
#                     label = NULL,
#                     choices = append(
#                       c("None", "Degree", "Indegree", "Outdegree", "Betweenness", "Closeness"),
#                       cont_attr_sel_lst()
#                     ),
#                     selected = "None")
# }, ignoreInit = TRUE)
# 
