# only runs on file upload or when collection view graph option selected
f_reset_filter_ctrls <- function(g) {
  # reset pruned list
  graph_rv$prune_nodes <- c()
  updateSelectInput(session, "pruned_nodes_select", choices = character(0))
  
  # clear text analysis plot list
  ta_rv$plot_data_list <- NULL
  
  if (is.null(g)) {
    # disable controls if no data
    disable_graph_controls()
    disableTextAnalysisControls()
    
    shinyjs::hide("visnet_html_dl_btn")
    shinyjs::hide("graph_graphml_dl_btn")
    
    return(NULL)
  }
  
  if (vcount(g) > 0) {
    # reset and enable graph filter controls
    reset_enable_graph_controls()
    
    shinyjs::show("graph_graphml_dl_btn")
    shinyjs::enable("graph_reseed_button")
    shinyjs::enable("graph_comp_slider")
    
    # f_update_comp_slider_range(g, isolate(input$graph_comp_type_sel))
    f_set_comp_ranges(g)
    comp_rv$mode <- "weak"
    f_set_comp_slider_range()
    
    dt_prev_sel$nodes <- c()
    shinyjs::reset("nbh_undo_button")
    
    # update the categorical attribute select box
    if (!is.null(graph_rv$graph_cats) && length(graph_rv$graph_cats) > 0) {
      shinyjs::reset("graph_cat_select")
      shinyjs::enable("graph_cat_select")
      
      category_choices <- c("All")
      category_choices <- append(category_choices, names(graph_rv$graph_cats))
      
      updateSelectInput(session, "graph_cat_select", choices = category_choices, selected = "All")
      
      shinyjs::reset("reset_on_change_check")
      shinyjs::enable("reset_on_change_check")
      
    } else {
      shinyjs::reset("graph_cat_select")
      shinyjs::disable("graph_cat_select")
      
      shinyjs::reset("graph_sub_cats_select")
      shinyjs::disable("graph_sub_cats_select")   
      
      shinyjs::reset("reset_on_change_check")
      shinyjs::disable("reset_on_change_check")      
    }
    
    # text analysis controls
    if (hasVosonTextData(g)) {
      ta_rv$has_text <- TRUE
      
      # reset and enable text analysis controls
      resetEnableTextAnalysisControls()
      
    } else {
      ta_rv$has_text <- FALSE
      
      # disable controls if no text data
      disableTextAnalysisControls()
    }
  }
}

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

# filter_rv <- reactiveVal()
# observeEvent(input$filter_order, {
#   req(any(c(input$graph_pruned_check,
#       input$graph_isolates_check,
#       input$graph_loops_edge_check,
#       input$graph_multi_edge_check,
#       input$graph_components_check,
#       input$graph_categorical_check)))
#   
#   filter_rv(input$filter_order)
# })

# apply all filters to graph data and return modified graph

# dependencies
# r_graph_base()
# graph_rv$prune_verts
# input$filter_order
# input$graph_cat_select
# input$graph_sub_cats_select

# input$graph_components_check
# isolate(input$graph_comp_type_sel)
# input$graph_comp_slider
# input$graph_comp_type_sel - maybe not isolated

# input$graph_multi_edge_check
# input$graph_loops_edge_check
# input$graph_isolates_check

r_graph_filtered <- reactive({
  g <- r_graph_base()
  
  pv <- graph_rv$prune_verts
  
  if (!is.null(g)) {
    f_order <- input$filter_order
    for (cmd in f_order) {
      if (cmd == "rm_pruned") {
        g <- applyPruneFilterSrv(g, graph_rv$prune_verts)
        
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
        if (input$graph_comps_chk == TRUE) {
          g <-
            applyComponentFilter(
              g,
              isolate(input$graph_comp_type_sel),
              input$graph_comp_slider
            )
        } else {
          f_set_comp_ranges(g)
          f_set_comp_slider_range()
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
  
  g
})

output$filter_rank_list <- renderUI({
  input$graph_filter_sort_reset
  rank_list(
    text = "Apply filters in order:",
    labels = list(
      "rm_pruned" = list(
        div(
          disabled(checkboxInput("graph_pruned_check", div("Prune Nodes", id = "graph_pruned_check_label"), FALSE)),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_isolates" = list(
        div(
          checkboxInput("graph_isolates_check", div("Remove Isolates", id = "graph_isolates_check_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_loops" = list(
        div(
          checkboxInput("graph_loops_edge_check", div("Remove Edge Loops", id = "graph_loops_edge_check_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_multiedges" = list(
        div(
          checkboxInput("graph_multi_edge_check", div("Merge Multiple Edges", id = "graph_multi_edge_check_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_components" = list(
        div(
          checkboxInput("graph_comps_chk", div("Components", id = "graph_comps_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_categories" = list(
        div(
          disabled(checkboxInput("graph_categorical_check", div("Categorical", id = "graph_categorical_check_label"), FALSE)),
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
  sapply(c("graph_cat_select", "graph_sub_cats_select"), function(x) shinyjs::reset(x))
})

observe({
  toggleClass(id = "graph_isolates_check_label", class = "txt_sel_blue", asis = TRUE, condition = input$graph_isolates_check)
  toggleClass(id = "graph_pruned_check_label", class = "txt_sel_blue", asis = TRUE, condition = input$graph_pruned_check)
  toggleClass(id = "graph_loops_edge_check_label", class = "txt_sel_blue", asis = TRUE, condition = input$graph_loops_edge_check)
  toggleClass(id = "graph_multi_edge_check_label", class = "txt_sel_blue", asis = TRUE, condition = input$graph_multi_edge_check)
  toggleClass(id = "graph_comps_chk_label", class = "txt_sel_blue", asis = TRUE, condition = input$graph_comps_chk)
  toggleClass(id = "graph_categorical_check_label", class = "txt_sel_blue", asis = TRUE, condition = input$graph_categorical_check)
})