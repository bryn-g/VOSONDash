fltr_state <- function() {
  list(
    "fltr_prune_chk" = FALSE,
    "fltr_iso_chk" = FALSE,
    "fltr_edge_loops_chk" = FALSE,
    "fltr_edge_multi_chk" = FALSE,
    "fltr_comp_chk" = FALSE,
    "fltr_cat_chk" = FALSE
  )
}

# apply all filters to graph data and return modified graph
filter_btn_txt_sel <- function(id, state) {
  shinyjs::toggleClass(id = id, class = "txt_sel_blue", asis = TRUE, condition = state)
}

f_init_fltr_state <- function() {
  sapply(fltr_state(), function(x) x = FALSE, simplify = FALSE, USE.NAMES = TRUE)
}

f_post_fltr_state <- function(state) {
  # sapply(names(state), function(x) {
  #   val <- state[[x]]
  #   if (isTruthy(val)) {
  #     filter_btn_txt_sel(paste(x, "_label"), val)
  #   }
  # })
}

r_graph_filter <- reactive({
  g <- r_graph_base()
  if (!isTruthy(g)) return(NULL)

  cat(file=stderr(), paste0(ts_utc(), " - r_graph_filter entry.\n"))
  
  fltr_state <- f_init_fltr_state()
  
  # filtering
  f_order <- input$fltr_order
  if (isTruthy(f_order)) {
    for (cmd in f_order) {
      if (cmd == "rm_pruned") {
  
        if (input$fltr_prune_chk) {
          g <- VOSONDash::filter_nodes(g, g_nodes_rv$pruned)
   
          fltr_state$fltr_prune_chk <- TRUE
        }
  
      } else if (cmd == "rm_categories") {
  
        if (input$fltr_cat_chk) {
          cat_sel <- g_nodes_rv$cat_selected
          cat_sub_sel <- g_nodes_rv$cat_sub_selected
  
          g <- VOSONDash::filter_cats(g, cat_sel, cat_sub_sel, cat_prefix = "")
          
          fltr_state$fltr_cat_chk <- TRUE
        }
        
      } else if (cmd == "rm_components") {
 
        if (input$fltr_comp_chk) {
          slider_vals <- input$comp_slider
          mode <- input$comp_mode_picker
          
          comp_range <- VOSONDash::get_comps_range(g, mode = mode)
          g_comps_rv$pre_comps <- comp_range
          
          id_vals <- input$comp_memb_sel
          if (!is.null(id_vals)) id_vals <- as.numeric(id_vals)
          
          g <- VOSONDash::filter_comps(g, mode, range = slider_vals, ids = id_vals)
          
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
        
      }
    }
  }
  
  # which filters ran
  f_post_fltr_state(fltr_state)
  
  # add measures of centrality
  g <- VOSONDash::add_centrality_measures(g)
  
  # update node and edge attributes
  # g_nodes_rv$attrs <- igraph::vertex_attr_names(g)
  # g_edges_rv$attrs <- igraph::edge_attr_names(g)
  
  # directed
  g_rv$dir <- igraph::is_directed(g)
  
  cat(file=stderr(), paste0(ts_utc(), " - r_graph_filter exit.\n"))
  
  g
})

# filter ui
output$filter_rank_list <- renderUI({
  input$graph_filter_sort_reset

  rank_list(
    text = "",
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
    options = sortable_options(swap = TRUE, animation = 150, handle = NULL)
  )
})
