disable_graph_controls <- function() {
  ui_controls <- c("graph_isolates_check",
                   "graph_multi_edge_check",
                   "graph_loops_edge_check",
                   "node_index_check",
                   "node_labels_check",
                   "node_sel_labels_check",
                   "graph_niter",
                   "graph_charge",
                   "graph_mass",
                   "graph_spr_len",
                   "graph_spr_const",
                   "graph_cat_select",
                   "graph_sub_cats_select",
                   "graph_node_size_select",
                   "graph_reseed_button",
                   "graph_layout_select", 
                   "igraph_spread_slider",
                   "igraph_node_base_size_slider",
                   "visgraph_node_base_size_slider",
                   "graph_comp_type_sel",
                   "graph_comp_slider")
  
  sapply(ui_controls, function(x) shinyjs::disable(x))
}

reset_enable_graph_controls <- function() {
  ui_controls <- c("graph_isolates_check", 
                   "graph_multi_edge_check", 
                   "graph_loops_edge_check",
                   "node_index_check",
                   "node_labels_check",
                   "node_sel_labels_check",
                   "graph_niter",
                   "graph_charge",
                   "graph_mass",
                   "graph_spr_len",
                   "graph_spr_const",
                   "graph_node_size_select",
                   "graph_node_size_slider",
                   "igraph_node_base_size_slider",
                   "graph_sub_cats_select", 
                   "graph_layout_select", 
                   "igraph_spread_slider",
                   "graph_comp_type_sel")
  
  sapply(ui_controls,
    function(x) {
      shinyjs::reset(x)
      shinyjs::enable(x)
  })
}

enable_igraph_controls <- function() {
  # shinyjs::disable("visnet_html_dl_btn")
  # shinyjs::hide("visnet_html_dl_btn")
  
  shinyjs::disable("visgraph_node_base_size_slider")
  
  ui_controls <- c("node_index_check",
                   "node_labels_check",
                   "node_sel_labels_check",
                   "graph_niter",
                   "graph_charge",
                   "graph_mass",
                   "graph_spr_len",
                   "graph_spr_const",                   
                   "graph_reseed_button",
                   "graph_layout_select",
                   "graph_node_size_select",
                   "graph_node_size_slider",
                   "igraph_spread_slider",
                   "igraph_node_base_size_slider",
                   "graph_multi_edge_check",
                   "graph_loops_edge_check")
  
  sapply(ui_controls, function(x) shinyjs::enable(x))
}

enable_visnet_controls <- function() {
  shinyjs::enable("visnet_html_dl_btn")
  shinyjs::show("visnet_html_dl_btn")
  
  shinyjs::enable("visgraph_node_base_size_slider")
  
  ui_controls <- c("igraph_spread_slider", "igraph_node_base_size_slider")
  
  sapply(ui_controls, function(x) shinyjs::disable(x))
}

# ------------

disableTextAnalysisControls <- function() {
  ui_controls <- c("ta_stopwords_check",
                   "ta_user_stopwords_input", 
                   "ta_user_stopwords_check",
                   "ta_stem_check",
                   "ta_wf_top_count",
                   "ta_wf_min_word_freq",
                   "ta_wc_min_word_freq",
                   "ta_wc_max_word_count",
                   "ta_cc_max_word_count")
  
  sapply(ui_controls, function(x) { shinyjs::disable(x) })
}

resetEnableTextAnalysisControls <- function() {
  ui_controls <- c("ta_stopwords_check",
                   "ta_user_stopwords_input", 
                   "ta_user_stopwords_check",
                   "ta_stem_check",
                   "ta_wf_top_count",
                   "ta_wf_min_word_freq",
                   "ta_wc_min_word_freq",
                   "ta_wc_max_word_count",
                   "ta_cc_max_word_count")
  
  sapply(ui_controls, function(x) { shinyjs::reset(x)
    shinyjs::enable(x) })
}


