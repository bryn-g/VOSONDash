

# filter out list of vertices from graph object
filter_nodesSrv <- function(g, selected_prune_verts) {
  # updateCheckboxInput(session, "fltr_prune_chk", value = FALSE)
  # if (length(selected_prune_verts) > 0) {
  #   verts <- which(V(g)$id %in% selected_prune_verts)
  #   g <- delete.vertices(g, verts) # selected_prune_verts
  #   updateCheckboxInput(session, "fltr_prune_chk", value = TRUE)
  # }
  # return(g)
  g
}

# filter_rv <- reactiveVal()
# observeEvent(input$fltr_order, {
#   req(any(c(input$fltr_prune_chk,
#       input$fltr_iso_chk,
#       input$fltr_edge_loops_chk,
#       input$fltr_edge_multi_chk,
#       input$graph_components_check,
#       input$fltr_cat_chk)))
#   
#   filter_rv(input$fltr_order)
# })

# apply all filters to graph data and return modified graph
filter_btn_txt_sel <- function(id, state) {
  shinyjs::toggleClass(id = id, class = "txt_sel_blue", asis = TRUE, condition = state)
}

r_graph_filtered <- reactive({
  g <- r_graph_base()
  
  if (!isTruthy(g)) return(NULL)

  # filtering
  f_order <- input$fltr_order
  
  # no_filters <- FALSE
  # if (all(sapply(list(
  #     input$fltr_prune_chk, input$fltr_iso_chk, input$fltr_edge_loops_chk,
  #     input$fltr_edge_multi_chk, input$fltr_comp_chk, input$fltr_cat_chk
  # ), isTruthy) == FALSE)) no_filters <- TRUE
  # 
  # if (!no_filters) {
    for (cmd in f_order) {
      if (cmd == "rm_pruned") {
        
        # if (length(g_nodes_rv$pruned)) g <- filter_nodesSrv(g, g_nodes_rv$pruned)
        # filter_btn_txt_sel("fltr_prune_chk_label", TRUE)
        
      } else if (cmd == "rm_categories") {
        if (input$fltr_cat_chk) {
          cat_sel <- g_nodes_rv$cat_selected
          sub_cat_sel <- g_nodes_rv$cat_sub_selected
          
          if (!("All" %in% cat_sel) & length(sub_cat_sel)) {
            filter_btn_txt_sel("fltr_cat_chk_label", TRUE)
            
            g <- VOSONDash::filter_cats(g, cat_sel, sub_cat_sel)
          } else {
            # updateCheckboxInput(session, "fltr_cat_chk", value = FALSE)
            filter_btn_txt_sel("fltr_cat_chk_label", FALSE)
          }
        }
      } else if (cmd == "rm_components") {
        
        filter_btn_txt_sel("fltr_comp_chk_label", TRUE)
        
        # filter component selection
        if (input$fltr_comp_chk) {
          # g <- filter_comps(g, isolate(input$comp_mode_sel), input$comp_slider)
        } else {
          # f_set_comp_ranges(g)
          # f_set_comp_slider_range()
        }
        
      } else if (cmd == "rm_multiedges" & input$fltr_edge_multi_chk == TRUE) {
        
        g <- VOSONDash::filter_edges(g, input$fltr_edge_multi_chk, FALSE)
        filter_btn_txt_sel("fltr_edge_multi_chk_label", TRUE)
        
      } else if (cmd == "rm_loops" & input$fltr_edge_loops_chk == TRUE) {
        
        g <- VOSONDash::filter_edges(g, FALSE, input$fltr_edge_loops_chk)
        filter_btn_txt_sel("fltr_edge_loops_chk_label", TRUE)
        
      } else if (cmd == "rm_isolates" & input$fltr_iso_chk == TRUE) {
        
        g <- VOSONDash::filter_nodes_iso(g)
        filter_btn_txt_sel("fltr_iso_chk_label", TRUE)
      }
    }   
  #}
  
  # measures of centrality
  g <- VOSONDash::add_centrality_measures(g)
  
  # update node attributes will also update labels
  g_nodes_rv$attrs <- igraph::vertex_attr_names(g)
  
  # directed
  g_rv$dir <- igraph::is_directed(g)
  
  # set labels
  if (isTruthy(g_nodes_rv$label_type)) {
    if (g_nodes_rv$label_type == "index") {
      igraph::V(g)$label <- sub("n", "", igraph::V(g)$id)
    } else if (g_nodes_rv$label_type == "attribute") {
      if (isTruthy(g_nodes_rv$label_selected)) {
        igraph::V(g)$label <- igraph::vertex_attr(g, g_nodes_rv$label_selected)
      }
    }
  }
  
  cat(file=stderr(), paste0("- r_graph_filtered - n:", igraph::gorder(g), ", e:", igraph::gsize(g), "\n"))
  
  g
})

chks <- c("fltr_prune_chk", "fltr_iso_chk", "fltr_edge_loops_chk", "fltr_edge_multi_chk", "fltr_comp_chk", "fltr_cat_chk")

# filter ui
output$filter_rank_list <- renderUI({
  input$graph_filter_sort_reset

  rank_list(
    text = "Apply filters in order:",
    labels = list(
      "rm_pruned" = list(
        div(
          checkboxInput("fltr_prune_chk", div("Prune Nodes", id = "fltr_prune_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_isolates" = list(
        div(
          checkboxInput("fltr_iso_chk", div("Remove Isolates", id = "fltr_iso_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_loops" = list(
        div(
          checkboxInput("fltr_edge_loops_chk", div("Remove Edge Loops", id = "fltr_edge_loops_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_multiedges" = list(
        div(
          checkboxInput("fltr_edge_multi_chk", div("Merge Multiple Edges", id = "fltr_edge_multi_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_components" = list(
        div(
          checkboxInput("fltr_comp_chk", div("Components", id = "fltr_comp_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "rm_categories" = list(
        div(
          checkboxInput("fltr_cat_chk", div("Categorical", id = "fltr_cat_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      )
    ),
    
    input_id = "fltr_order",
    options = sortable_options(swap = TRUE)
  )
})

# observeEvent(input$graph_filter_sort_reset, {
#   sapply(c("cat_sel", "cat_sub_sel"), function(x) shinyjs::reset(x))
# })
# 
