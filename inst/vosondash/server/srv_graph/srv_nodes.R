# graph nodes reactive variables
g_nodes_rv <- reactiveValues(
  attrs = NULL,
  
  labels = NULL,
  label_type = "none",
  label_selected = NULL,
  
  cats = NULL,
  cat_selected = "All",
  cat_sub_selected = "All",
  
  properties = NULL,
  
  pruned = NULL
)

# base graph node attribute list
r_node_attr_lst <- reactive({
  g <- req(r_graph_base())
  
  node_attr <- igraph::vertex_attr_names(g)
  init_attr_sel <- ifelse("imported_label" %in% node_attr, "imported_label", "id")
  node_attr <- sort(node_attr[!node_attr %in% c("label")])
  
  list(attrs = node_attr, sel = init_attr_sel)
})

# reset node size slider when changed to none
observeEvent(input$node_size_sel, {
  if (input$node_size_sel == "None") shinyjs::reset("node_size_slider")
}, ignoreInit = TRUE)

observeEvent(input$reset_node_attrs_btn, {
  if (isTruthy(input$reset_node_attrs_btn)) {
    set_ctrl_state(
      c("node_color",
        "igraph_node_base_size_slider",
        "visnet_node_base_size_slider",
        "node_use_g_cols_chk",
        "node_size_sel",
        "node_size_slider",
        "node_label_prop_chk",
        "mtdn_img_opts_chk",
        "node_mtdn_img_chk",
        "node_mtdn_img_sq_chk",
        "node_mtdn_img_bord_chk"), "reset")
  }
}, ignoreInit = TRUE)

observeEvent(input$reset_node_labels_btn, {
  if (isTruthy(input$reset_node_labels_btn)) {
    set_ctrl_state(
      c("node_index_chk",
        "node_labels_chk",
        "node_label_sel",
        "node_sel_labels_chk",
        "visnet_id_sel_chk",
        "macos_font_chk",
        "node_label_color",
        "node_label_size",
        "node_label_rot",
        "node_label_dist"), "reset")
  }
}, ignoreInit = TRUE)
