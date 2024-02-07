# graph reactive variables
g_rv <- reactiveValues(
  data = NULL,
  dir = NULL,
  seed = NULL,
  is_igraph = FALSE
)

g_pkg_rv <- reactiveValues(
  files = NULL,
  meta_files = NULL
)

# graph metadata reactive variables
g_meta_rv <- reactiveValues(
  data = NULL
)

# graph nodes reactive variables
g_nodes_rv <- reactiveValues(
  attrs = NULL,
  labels = NULL,
  label_type = "none",
  label_selected = NULL,
  cats = NULL,
  cat_selected = "All",
  cat_sub_selected = "All",
  pruned = NULL
)

# plot reactive variables
g_plot_rv <- reactiveValues(
  height = gbl_plot_height,
  height_legend = 42,
  width = gbl_plot_width
)

# if data becomes invalid then disable the controls
observeEvent(g_rv$is_igraph, {
  if (!isTruthy(g_rv$is_igraph)) {
    dash_logger("igraph data object invalid.")
    
    disable_g_ctrls()
    
  } else {
    dash_logger(paste0("igraph data object loaded: ", g_meta_rv$data$name))
  }
}, ignoreInit = TRUE)

# reset node size slider when changed to none
observeEvent(input$node_size_sel, {
  if (input$node_size_sel == "None") shinyjs::reset("node_size_slider")
}, ignoreInit = TRUE)

# normalize continuous values
norm_values <- function(x) {
  # all values the same
  if (var(x) == 0) return(rep(0.1, length(x)))
  
  min_x <- min(x)
  diff_x <- max(x) - min_x
  s <- sapply(x, function(y) ((y - min_x) / diff_x))
}

# event for graph data set by upload or collection
observeEvent(g_rv$data, {
  g_rv$is_igraph <- ifelse((isTruthy(g_rv$data) & ("igraph" %in% class(g_rv$data))), TRUE, FALSE)
  
  updateNavbarPage(session, "nav_sel_tab_id", selected = "network_graphs_tab")
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)

# base graph node attribute list
r_node_attr_lst <- reactive({
  g <- req(r_graph_base())
  
  node_attr <- igraph::vertex_attr_names(g)
  init_attr_sel <- ifelse("imported_label" %in% node_attr, "imported_label", "id")
  node_attr <- sort(node_attr[!node_attr %in% c("label")])
  
  list(attrs = node_attr, sel = init_attr_sel)
})

# set the base graph object from initial processing
r_graph_base <- reactive({
  req(g_rv$data)
  
  # clean base graph
  g <- g_rv$data
  
  # check igraph
  if (!"igraph" %in% class(g)) return(NULL)
  
  g <- f_set_id_and_label(g)
  
  g
})

observeEvent(r_graph_base(), {
  g <- r_graph_base()
  
  dash_logger("graph base changed.")
  
  g_nodes_rv$cats <- getNodeCategories(g)
  
  g_rv$seed <- sample(1:20000, 1)
  
  g_nodes_rv$pruned <- c()
  dt_prev_sel$nodes <- c()
  
  ta_rv$plot_data_list <- NULL
  
  # reset / enable / disable ctrls
  reset_enable_g_ctrls()
})

f_unchk_disable_cat_fltr <- function() {
  updateCheckboxInput(session, "fltr_cat_chk", value = FALSE)
  filter_btn_txt_sel("fltr_cat_chk_label", FALSE)
  # shinyjs::disable("fltr_cat_chk")
}

observeEvent(g_nodes_rv$cats, {
  if (!isTruthy(g_nodes_rv$cats)) f_unchk_disable_cat_fltr()
  if (length(names(g_nodes_rv$cats)) < 1) f_unchk_disable_cat_fltr()
    
  shinyjs::enable("cat_sel")
  cats <- append("All", names(g_nodes_rv$cats))
  updateSelectInput(session, "cat_sel", choices = cats, selected = "All")
  
  updateSelectInput(session, "cat_sub_sel", choices = "All", selected = "All")
  shinyjs::disable("cat_sub_sel")
  
  g_nodes_rv$cat_selected <- "All"
  g_nodes_rv$cat_sub_selected <- "All"
  
  shinyjs::enable("fltr_cat_chk")
  updateCheckboxInput(session, "fltr_cat_chk", value = FALSE)
})

observeEvent(input$cat_sel, {
  req(g_nodes_rv$cats, input$cat_sel)
  
  if (input$cat_sel != "All") {
    shinyjs::enable("cat_sub_sel")
    sub_cats <- append("All", g_nodes_rv$cats[[input$cat_sel]])
    updateSelectInput(session, "cat_sub_sel", choices = sub_cats, selected = "All")
    
    g_nodes_rv$cat_selected <- input$cat_sel
    g_nodes_rv$cat_sub_selected <- "All"
  } else {
    shinyjs::disable("cat_sub_sel")
    updateSelectInput(session, "cat_sub_sel", choices = "All", selected = "All")
  }

}, ignoreInit = TRUE)

observeEvent(input$cat_sub_sel, {
  req(g_nodes_rv$cats, input$cat_sel, input$cat_sub_sel)
  
  g_nodes_rv$cat_sub_selected <- input$cat_sub_sel
}, ignoreInit = TRUE)



# -----------------------
f_init_graph_ctrls2 <- function(g) {
  # reset pruned list
  g_nodes_rv$pruned <- c()
  updateSelectInput(session, "prune_nodes_sel", choices = character(0))
  
  # reset text analysis plot list
  ta_rv$plot_data_list <- NULL
  
  # is graph data present
  if (is.null(g)) {
    # disable controls if no data
    disable_graph_controls()
    disable_ta_ctrls()
    
    # shinyjs::hide("visnet_html_dl_btn")
    # shinyjs::hide("graph_graphml_dl_btn")
    
    return(NULL)
  }
  
  # if graph nodes
  if (igraph::gorder(g) > 0) {
    # reset and enable graph filter controls
    reset_enable_graph_controls()
    
    # shinyjs::show("graph_graphml_dl_btn")
    # shinyjs::enable("graph_reseed_btn")
    # shinyjs::enable("comp_slider")
    
    # f_update_comp_slider_range(g, isolate(input$comp_type_sel))
    # f_set_comp_ranges(g)
    # comp_rv$mode <- "weak"
    # f_set_comp_slider_range()
    
    dt_prev_sel$nodes <- c()
    shinyjs::reset("nbh_undo_btn")
    
    # update the categorical attribute select box
    # if (!is.null(g_nodes_rv$cats) && length(g_nodes_rv$cats) > 0) {
    #   shinyjs::reset("cat_sel")
    #   shinyjs::enable("cat_sel")
    #   
    #   category_choices <- c("All")
    #   category_choices <- append(category_choices, names(g_nodes_rv$cats))
    #   
    #   updateSelectInput(session, "cat_sel", choices = category_choices, selected = "All")
    #   
    #   shinyjs::reset("reset_on_change_check")
    #   shinyjs::enable("reset_on_change_check")
    #   
    # } else {
    #   shinyjs::reset("cat_sel")
    #   shinyjs::disable("cat_sel")
    #   
    #   shinyjs::reset("cat_sub_sel")
    #   shinyjs::disable("cat_sub_sel")   
    #   
    #   shinyjs::reset("reset_on_change_check")
    #   shinyjs::disable("reset_on_change_check")      
    # }
    
    # text analysis controls
    if (hasVosonTextData(g)) {
      ta_rv$has_text <- TRUE
      
      # reset and enable text analysis controls
      reset_enable_ta_ctrls()
      
    } else {
      ta_rv$has_text <- FALSE
      
      # disable controls if no text data
      disable_ta_ctrls()
    }
  }
  cat(file=stderr(), "running f_init_graph_ctrls\n")
  
}

# init disable tabs with css
# addCssClass(selector = "a[data-value = 'metrics_tab_panel']", class = "inactive_menu_link")
# addCssClass(selector = "a[data-value = 'assort_tab_panel']", class = "inactive_menu_link")

# observeEvent(r_graph_base , {
#   r_node_cat_lst()
#   f_reset_filter_ctrls()
# })

# base graph node category list
# r_node_cat_lst <- reactive({
#   g <- req(r_graph_base())
#   
#   getNodeCategories(g)
# })



# r_graph_labels <- reactive({
#   g <- req(g_rv$data)
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
# }) # > bindEvent(g_rv$data)

# # when graphml data loaded or changed
# observeEvent(r_graph_labels(), { #g_rv$data, {
#   g <- req(r_graph_labels())
#   #g <- req(g_rv$data)
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
#   #g_rv$dir <- igraph::is_directed(g)
#   
#   # enable network metrics tab
#   #removeCssClass(selector = "a[data-value = 'metrics_tab_panel']", class = "inactive_menu_link")
#   
#   # set directed
#   g_rv$dir <- igraph::is_directed(g)
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
#   if (!is.null(g_rv$data)) {
#     g <- g_rv$data
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
#     g <- applyPruneFilterSrv(g, g_rv$prune_nodes)
#     g <- applyCategoricalFilters(g, input$cat_sel, input$cat_sub_sel)
#     # isolate as comp_type_sel has event
#     g <- applyComponentFilter(g, isolate(input$comp_type_sel), input$comp_slider)    
#     g <- applyGraphFilters(g, input$fltr_iso_chk, input$fltr_edge_multi_chk, 
#                            input$fltr_edge_loops_chk)
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
# observeEvent({ input$comp_type_sel
#   input$cat_sub_sel
#   g_rv$prune_nodes
#   input$reset_on_change_check }, {
#     
#     g <- g_rv$data
#     g <- applyPruneFilterSrv(g, g_rv$prune_nodes)
#     
#     if (input$reset_on_change_check == TRUE) {
#       g <- applyCategoricalFilters(g, input$cat_sel, input$cat_sub_sel)
#     }
#     
#     f_update_comp_slider_range(g, input$comp_type_sel)
#   }, ignoreInit = TRUE)

# r_comp_summary_txt <- reactive({
#   g <- graphFilters()
#   
#   output <- c()
#   
#   if (!is.null(g)) {
#     graph_clusters <- components(g, mode = isolate(input$comp_type_sel))
#     
#     output <- append(output, paste0("Components (", isolate(input$comp_type_sel), "): ", 
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
#   g <- g_rv$data
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
#   freezeReactiveValue(input, "node_size_sel")
#   updateSelectInput(session,
#                     "node_size_sel",
#                     label = NULL,
#                     choices = append(
#                       c("None", "Degree", "Indegree", "Outdegree", "Betweenness", "Closeness"),
#                       cont_attr_sel_lst()
#                     ),
#                     selected = "None")
# }, ignoreInit = TRUE)
# 
