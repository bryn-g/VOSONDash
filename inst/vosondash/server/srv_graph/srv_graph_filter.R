fltr_state <- function()
  list(
    "fltr_prune_chk" = FALSE,
    "fltr_iso_chk" = FALSE,
    "fltr_edge_loops_chk" = FALSE,
    "fltr_edge_multi_chk" = FALSE,
    "fltr_comp_chk" = FALSE,
    "fltr_cat_chk" = FALSE
  )


# apply all filters to graph data and return modified graph
filter_btn_txt_sel <- function(id, state) {
  shinyjs::toggleClass(id = id, class = "txt_sel_blue", asis = TRUE, condition = state)
}

f_init_fltr_state <- function() {
  sapply(fltr_state, function(x) x = FALSE, simplify = FALSE, USE.NAMES = TRUE)
}

f_post_fltr_state <- function(fltr_state) {
  req(fltr_state)
  sapply(names(fltr_state), function(x) {
    val <- fltr_state[[x]]
    if (isTruthy(val)) {
      filter_btn_txt_sel(paste(x, "_label"), val)
      dbg("f_post_fltr_state", paste0(x, " = TRUE"))
    }
  })
}

r_graph_filter <- reactive({
  g <- r_graph_base()
  if (!isTruthy(g)) return(NULL)

  fltr_state <- f_init_fltr_state()
  
  # filtering
  f_order <- input$fltr_order
  if (isTruthy(f_order)) {
    dbg("f_order", f_order)
    for (cmd in f_order) {
      dbg("cmd", cmd)
      if (cmd == "rm_pruned") {
  
        # to do
        if (input$fltr_prune_chk) {
          fltr_state$fltr_prune_chk <- TRUE
        }
  
      } else if (cmd == "rm_categories") {
  
        if (input$fltr_cat_chk) {
          cat_sel <- g_nodes_rv$cat_selected
          cat_sub_sel <- g_nodes_rv$cat_sub_selected
  
          g <- VOSONDash::filter_cats(g, cat_sel, cat_sub_sel)
          fltr_state$fltr_cat_chk <- TRUE
        }
        
      } else if (cmd == "rm_components") {
      
        if (input$fltr_comp_chk) {
          comp_slider <- g_comps_rv$slider
          mode <- g_comps_rv$mode
  
          g_comps_rv$pre_comps <- f_get_comp_ranges(g, mode = mode)
          # could update range
          
          g <- VOSONDash::filter_comps(g, mode, comp_slider)
          fltr_state$fltr_comp_chk <- TRUE
        } else {
          g_comps_rv$pre_comps <- NULL
        }
        
      } else if (cmd == "rm_multiedges") {
        
        if (input$fltr_edge_multi_chk) {
          g <- VOSONDash::filter_edges(g, input$fltr_edge_multi_chk, FALSE)
          fltr_state$fltr_edge_multi_chk <- TRUE
        }
        
      } else if (cmd == "rm_loops") {
        
        if (input$fltr_edge_loops_chk) {
          g <- VOSONDash::filter_edges(g, FALSE, input$fltr_edge_loops_chk)
          fltr_state$fltr_edge_loops_chk <- TRUE
        }
        
      } else if (cmd == "rm_isolates") {
        
        if (input$fltr_iso_chk) {
          g <- VOSONDash::filter_nodes_iso(g)
          fltr_state$fltr_iso_chk <- TRUE
        }
        
      } else {
        dbg("f_order", paste0("cmd = ", cmd))
      }
    }
  }
  
  f_post_fltr_state(fltr_state)
  
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
  
  cat(file=stderr(), paste0("- r_graph_filter - n:", igraph::gorder(g), ", e:", igraph::gsize(g), "\n"))
  
  g
})

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
