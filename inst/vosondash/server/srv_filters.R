# filter out list of vertices from graph object
applyPruneFilterSrv <- function(g, selected_prune_verts) {
  updateCheckboxInput(session, "graph_pruned_check", value = FALSE)
  if (length(selected_prune_verts) > 0) {
    verts <- which(V(g)$id %in% selected_prune_verts)
    g <- delete.vertices(g, verts) # selected_prune_verts
    updateCheckboxInput(session, "graph_pruned_check", value = TRUE)
  }
  return(g)
}

# apply all filters to graph data and return modified graph
graphFilters <- reactive({
  g <- NULL
  
  if (!is.null(ng_rv$graph_data)) {
    g <- ng_rv$graph_data
    
    # ----
    # add vertex ids and labels if not present
    attr_v <- vertex_attr_names(g)
    if (!("id" %in% attr_v)) {
      V(g)$id <- paste0("n", as.numeric(V(g)) - 1) # n0, n1 ..
    }
    
    if ("label" %in% attr_v) {
      # replace empty string labels
      V(g)$label <- ifelse(nchar(V(g)$label) > 0, V(g)$label, "-")
    } else {
      # if no labels set label to vertex name
      V(g)$label <- ifelse(nchar(V(g)$name) > 0, V(g)$name, "-")
    }
    # ----
    
    f_order <- input$filter_order
    for (cmd in f_order) {
      if (cmd == "rm_pruned") {
        g <- applyPruneFilterSrv(g, ng_rv$prune_verts)
        
      } else if (cmd == "rm_categories") {
        if (!("All" %in% input$graph_sub_cats_select) & length(input$graph_sub_cats_select)) {
          updateCheckboxInput(session, "graph_categorical_check", value = TRUE)
        } else {
          updateCheckboxInput(session, "graph_categorical_check", value = FALSE)
        }
        g <-
          applyCategoricalFilters(g,
                                  input$graph_cat_select,
                                  input$graph_sub_cats_select)
        
      } else if (cmd == "rm_components") {
        # filter component selection
        if (input$graph_components_check == TRUE) {
          g <-
            applyComponentFilter(
              g,
              isolate(input$graph_component_type_select),
              input$graph_component_slider
            )
        } else {
          setComponentRanges(g, input$graph_component_type_select)
          setComponentSliders()
        }
        
      } else if (cmd == "rm_multiedges" &
                 input$graph_multi_edge_check == TRUE) {
        g <-
          applyGraphFilters(g,
                            input$graph_multi_edge_check,
                            input$graph_loops_edge_check)
        
      } else if (cmd == "rm_loops" &
                 input$graph_loops_edge_check == TRUE) {
        g <-
          applyGraphFilters(g,
                            input$graph_multi_edge_check,
                            input$graph_loops_edge_check)
        
      } else if (cmd == "rm_isolates" &
                 input$graph_isolates_check == TRUE) {
        g <- igraph::delete_vertices(g, degree(g) == 0)
      }
    }
    
    g <- addAdditionalMeasures(g)
    
    # enable network metrics tab
    removeCssClass(selector = "a[data-value = 'network_metrics_tab']", class = "inactive_menu_link")
  }
  
  return(g)
})

output$filter_rank_list <- renderUI({
  input$graph_filter_sort_reset
  rank_list(
    text = "Apply filters in order:",
    labels = list(
      "rm_pruned" = list(
        div(
          disabled(checkboxInput("graph_pruned_check", "Prune Nodes", FALSE)),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_isolates" = list(
        div(
          checkboxInput("graph_isolates_check", "Remove Isolates", FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_loops" = list(
        div(
          checkboxInput("graph_loops_edge_check", "Remove Loops", FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_multiedges" = list(
        div(
          checkboxInput("graph_multi_edge_check", "Merge Multiple Edges", FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_components" = list(
        div(
          checkboxInput("graph_components_check", "Components", FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_categories" = list(
        div(
          disabled(checkboxInput("graph_categorical_check", "Categorical", FALSE)),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      )
    ),
    input_id = "filter_order",
    options = sortable_options(swap = TRUE)
  )
})

observeEvent(input$graph_filter_sort_reset, {
  sapply(c("graph_cat_select", "graph_sub_cats_select"), function(x) {
    shinyjs::reset(x)
  })
})
