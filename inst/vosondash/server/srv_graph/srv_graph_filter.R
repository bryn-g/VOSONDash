g_filter_rv <- reactiveValues(
  active = NULL,
  directed = NULL,
  update = TRUE
)

# apply all filters to graph data and return modified graph
# filter_btn_txt_sel <- function(id, state) {
#   shinyjs::toggleClass(id = id, class = "txt_sel_blue", asis = TRUE, condition = state)
# }

observeEvent(input$fltr_cat_chk, {
  if (input$fltr_cat_chk) updateTabsetPanel(session, "node_filters_tabset", selected = "Categorical")
})

observeEvent(input$fltr_comp_chk, {
  if (input$fltr_comp_chk) updateTabsetPanel(session, "node_filters_tabset", selected = "Components")
})

observeEvent(input$fltr_prune_chk, {
  if (input$fltr_prune_chk) updateTabsetPanel(session, "node_filters_tabset", selected = "Prune")
})

# consolidate filter changes into single event
observeEvent({
  input$fltr_order
  input$fltr_prune_chk
  input$fltr_iso_chk
  input$fltr_edge_loops_chk
  input$fltr_edge_multi_chk
  input$fltr_comp_chk
  input$fltr_cat_chk
  }, {
    # requires filter ui to be ready
    fltr_order <- req(input$fltr_order)
    
    # get filter states
    fltr_states <- list(
      "fltr_prune" = input$fltr_prune_chk,
      "fltr_iso" = input$fltr_iso_chk,
      "fltr_edge_loops" = input$fltr_edge_loops_chk,
      "fltr_edge_multi" = input$fltr_edge_multi_chk,
      "fltr_comp" = input$fltr_comp_chk,
      "fltr_cat" = input$fltr_cat_chk
    )
    
    # order filters and keep only active
    fltrs <- tibble::tibble(id = fltr_order) |>
      dplyr::left_join(tibble::tibble(id = names(fltr_states), state = fltr_states), by = "id") |>
      dplyr::filter(state == TRUE) |>
      dplyr::pull(id)
    
    if (length(fltrs)) {
      g_filter_rv$active <- fltrs
    } else {
      g_filter_rv$active <- NULL
    }
    
}, ignoreInit = TRUE, ignoreNULL = TRUE)

r_graph_filter <- reactive({
  # effectively disable reactivity for some events
  if (!g_filter_rv$update) {
    g_filter_rv$update <- TRUE
    cat(file=stderr(), paste0("r_graph_filter: skip update\n"))
    req(FALSE, cancelOutput = TRUE)
  }
  
  g <- r_graph_base()
  
  # init with null to dependents
  if (!isTruthy(g)) {
    cat(file=stderr(), paste0("r_graph_filter: return null\n"))
    return(NULL)
  }
  
  # active filters
  active_fltrs <- g_filter_rv$active
  
  # no active filters
  if (is.null(active_fltrs)) {
    cat(file=stderr(), paste0("r_graph_filter: no filters\n"))
    return(f_recalc_measures(g))
  }

  cat(file=stderr(), paste0("r_graph_filter: processing\n"))
  
  # apply active filters
  for (f in active_fltrs) {
    
    if (f == "fltr_prune") {
      nodes <- g_prune_rv$action

      if (!is.null(nodes) && nrow(nodes)) {
        ids <- nodes |> dplyr::pull(name)
        g <- VOSONDash::filter_nodes(g, ids)
        
        # set all removed
        rm_chk <- igraph::V(g)$name[which(igraph::V(g)$name %in% ids)]
        g_prune_rv$nodes <- isolate(g_prune_rv$nodes) |> dplyr::mutate(status = ifelse(!name %in% rm_chk, "removed", "failed"), idx = ifelse(!name %in% rm_chk, (idx*-1), idx))
        # g_prune_rv$action <- NULL
      }

    } else if (f == "fltr_cat") {
      cat_sel <- g_nodes_rv$cat_selected
      cat_sub_sel <- g_nodes_rv$cat_sub_selected

      g <- VOSONDash::filter_cats(g, cat_sel, cat_sub_sel, cat_prefix = "")
      
    } else if (f == "fltr_comp") {
      mode <- input$comp_mode_picker

      # set pre-filter component state
      comp_range <- VOSONDash::get_comps_range(g, mode = mode)
      g_comps_rv$pre_comps <- comp_range
      
      # filter components
      id_vals <- input$comp_memb_sel
      if (!is.null(id_vals)) id_vals <- as.numeric(id_vals)
      
      # does not get set to null - for colors
      selected_range <- g_comps_rv$fltr_range

      if (!is.null(id_vals) || !is.null(selected_range)) {
        g <- VOSONDash::filter_comps(g, mode, range = selected_range, ids = id_vals)
      }
      
    } else if (f == "fltr_edge_multi") {
      g <- VOSONDash::filter_edges(g, TRUE, FALSE)
      
    } else if (f == "fltr_edge_loops") {
      g <- VOSONDash::filter_edges(g, FALSE, TRUE)
      
    } else if (f == "fltr_iso") {
      g <- VOSONDash::filter_nodes_iso(g)
    }
  }
  
  g <- f_recalc_measures(g)
})

f_recalc_measures <- function(g) {
  g <- VOSONDash::add_cent_measures(g)
  g_filter_rv$directed <- igraph::is_directed(g)

  g # tidygraph::as_tbl_graph()
}

# filter ui
output$filter_rank_list <- renderUI({
  input$graph_filter_sort_reset

  rank_list(
    text = "",
    labels = list(
      "fltr_prune" = list(
        div(
          checkboxInput("fltr_prune_chk", div("Prune Nodes", id = "fltr_prune_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "fltr_iso" = list(
        div(
          checkboxInput("fltr_iso_chk", div("Remove Isolates", id = "fltr_iso_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "fltr_edge_loops" = list(
        div(
          checkboxInput("fltr_edge_loops_chk", div("Remove Edge Loops", id = "fltr_edge_loops_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "fltr_edge_multi" = list(
        div(
          checkboxInput("fltr_edge_multi_chk", div("Merge Multiple Edges", id = "fltr_edge_multi_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "fltr_comp" = list(
        div(
          checkboxInput("fltr_comp_chk", div("Components", id = "fltr_comp_chk_label"), FALSE),
          class = "div_inline"
        ),
        div(icon("sort"), style = "float: right;")
      ),
      "fltr_cat" = list(
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
