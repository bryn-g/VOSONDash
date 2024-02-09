# unused - node_attr_reset_btn
# unused - node_labels_reset_btn
# dt - dt_nodes_rows_selected
# visnet input - vis_node_select, vis_nbh_node_select

# - grph filters

# state change based on action
set_ctrl_state <- function(x, action) {
  f_state <- eval(parse(text = paste0("shinyjs::", action)))
  sapply(x, function(y) f_state(y))
}

overlay_ctrls <- function() {
  c("overlay_dl_btns_chk",
    "overlay_legend_chk",
    "overlay_summary_chk"
  )
}

# igraph plot ctrls
igraph_ctrls <- function() {
  c("igraph_spread_slider",
    "igraph_node_base_size_slider")
}

# visnet plot ctrls
visnet_ctrls <- function() {
  c("visnet_node_base_size_slider")
}

# layout ctrls
layout_ctrls <- function() {
  c("graph_reseed_btn",
    "graph_seed_input",
    "graph_seed_set_btn",
    "graph_layout_select",
    "graph_niter",
    "graph_charge",
    "graph_mass",
    "graph_spr_len",
    "graph_spr_const")
}

# node ctrls
node_ctrls <- function() {
  c("node_index_chk",
    "node_labels_chk",
    "node_label_prop_chk",
    "node_sel_labels_chk",
    "node_use_g_cols_chk",
    "node_size_sel",
    "node_size_slider",
    "node_mtdn_img_chk",
    "node_mtdn_img_sq_chk",
    "node_mtdn_img_bord_chk")
}

# node nbh ctrls
node_nbh_ctrls <- function() {
  c("nbh_undo_btn",
    "nbh_reset_btn",
    "nbh_desel_btn",
    "nbh_sel_btn",
    "nbh_prune_unsel",
    "nbh_order_sel")
}

# node pruning ctrls
node_prune_ctrls <- function() {
  c("prune_nodes_sel",
    "prune_reset_btn",
    "prune_desel_rows_btn",
    "prune_nodes_ret_btn")
}

# edge ctrls
edge_ctrls <- function() {
  c("edge_labels_chk",
    "edge_label_sel",
    "edge_label_color",
    "edge_label_size")
}

# graph filter ctrls
filter_ctrls <- function() {
  c("fltr_order",
    "fltr_prune_chk",
    "fltr_iso_chk",
    "fltr_edge_multi_chk",
    "fltr_edge_loops_chk",
    "fltr_cat_chk",
    "fltr_comp_chk")
}

# component filter ctrls
filter_comp_ctrls <- function() {
  c("comp_mode_sel",
    "comp_slider",
    "comp_recalc")
}

# category filter ctrls
filter_cat_ctrls <- function() {
  c("cat_sel",
    "cat_sub_sel")
}

text_analysis_ctrls <- function() {
  c("ta_stopwords_check",
    "ta_user_stopwords_input", 
    "ta_user_stopwords_check",
    "ta_stem_check",
    "ta_wf_top_count",
    "ta_wf_min_word_freq",
    "ta_wc_min_word_freq",
    "ta_wc_max_word_count",
    "ta_cc_max_word_count")  
}

disable_g_ctrls <- function() {
  dash_logger("disable graph controls.")
  x <- c(
    layout_ctrls(),
    igraph_ctrls(),
    node_ctrls(),
    node_prune_ctrls(),
    node_nbh_ctrls(),
    edge_ctrls(),
    filter_ctrls(),
    filter_comp_ctrls(),
    filter_cat_ctrls(),
    
    overlay_ctrls()
  )
  set_ctrl_state(x, "disable")
  disable_visnet_ctrls()
}

reset_enable_g_ctrls <- function() {
  dash_logger("reset enable graph controls.")
  x <- c(
    layout_ctrls(),
    igraph_ctrls(),
    visnet_ctrls(),
    node_ctrls(),
    node_prune_ctrls(),
    node_nbh_ctrls(),
    edge_ctrls(),
    filter_ctrls(),
    filter_comp_ctrls(),
    filter_cat_ctrls(),
    
    overlay_ctrls()
  )
  set_ctrl_state(x, "reset")
  set_ctrl_state(x, "enable")
}

disable_visnet_ctrls <- function() {
  set_ctrl_state(c(visnet_ctrls(), "visnet_html_dl_btn"), "disable")
  set_ctrl_state("visnet_html_dl_btn", "hide")
}

disable_igraph_ctrls <- function() {
  set_ctrl_state(igraph_ctrls(), "disable")
}

enable_igraph_ctrls <- function() {
  disable_visnet_ctrls()
  set_ctrl_state(igraph_ctrls(), "enable")
}

enable_visnet_ctrls <- function() {
  disable_igraph_ctrls()
  set_ctrl_state(c(visnet_ctrls(), "visnet_html_dl_btn"), "enable")
  set_ctrl_state("visnet_html_dl_btn", "show")
}

# disable text analysis ctrls
disable_ta_ctrls <- function() set_ctrl_state(text_analysis_ctrls(), "disable")

# reset enable text analysis ctrls
reset_enable_ta_ctrls <- function() {
  set_ctrl_state(text_analysis_ctrls(), "reset")
  set_ctrl_state(text_analysis_ctrls(), "enable")
}
